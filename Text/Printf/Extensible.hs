{-# LANGUAGE CPP, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{- Thanks to http://stackoverflow.com/questions/11171325/ for the above. -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf.Extensible
-- Copyright   :  (c) Lennart Augustsson and Bart Massey 2013
-- License     :  BSD-style (see the file LICENSE in this distribution)
--
-- Maintainer  :  bart@cs.pdx.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- A C @printf(3)@-like formatter. This version has been extended by
-- Bart Massey as per the recommendations of John Meacham and
-- Simon Marlow
-- <http://comments.gmane.org/gmane.comp.lang.haskell.libraries/4726>
-- to support extensible formatting for new datatypes.
-- It has also been extended to support most C @printf(3)@ syntax.
-----------------------------------------------------------------------------

module Text.Printf.Extensible (
-- * Printing Functions
   printf, hPrintf,
-- * Extending To New Types
--
-- | This 'printf' can be extended to format types
-- other than those provided for by default. This
-- is done by instancing 'PrintfArg' and providing
-- a 'toField' for the type. It is possible to
-- provide a 'parseFormat' to process type-specific
-- modifiers, but the default instance is usually
-- the best choice.
--
-- For example:
--
-- > instance PrintfArg () where
-- >   toField x fmt | fmtChar fmt == 'u' =
-- >     formatString "()" (fmt { fmtChar = 's', fmtPrecision = Nothing })
-- >   toField _ _ = error "invalid format character"
-- >
-- > main :: IO ()
-- > main = printf "<%-3.1u>\n" ()
--
-- prints \"@<() >@\". Note the use of 'formatString' to
-- take care of field formatting specifications in a convenient
-- way.
   PrintfArg(..),
   FieldFormatter,
   FieldFormat(..),
   FormatAdjustment(..), FormatSign(..),
-- ** Handling Type-specific Modifiers
--
-- | In the unlikely case that modifier characters of
-- some kind are desirable for a user-provided type,
-- a 'ModifierParser' can be provided to process these
-- characters. The resulting modifiers will appear in
-- the 'FieldFormat' for use by the type-specific formatter.
   ModifierParser, FormatParse(..),
-- ** Standard Formatters
--
-- | These formatters for standard types are provided for
-- convenience in writting new type-specific formatters:
-- a common pattern is to throw to 'formatString' or
-- 'formatInteger' to do most of the format handling for
-- a new type.
   formatString, formatChar, formatInt,
   formatInteger, formatRealFloat,
-- * Implementation Internals
-- | These types are needed for implementing processing
-- variable numbers of arguments to 'printf' and 'hPrintf'.
-- Their implementation is intentionally not visible from
-- this module. If you attempt to pass an argument of a type
-- which is not an instance of the appropriate class to
-- 'printf' or 'hPrintf', then the compiler will report it
-- as a missing instance of 'PrintfArg'.  (All 'PrintfArg'
-- instances are 'PrintfType' instances.)
  PrintfType, HPrintfType
) where

import Prelude
import Data.Char
import Data.Int
import Data.List (stripPrefix)
import Data.Map as M hiding (adjust, map)
import Data.Word
import Numeric(showEFloat, showFFloat, showGFloat)
import System.IO

-------------------

-- | Format a variable number of arguments with the C-style formatting string.
-- The return value is either 'String' or @('IO' ())@.
--
-- The format string consists of ordinary characters and /conversion
-- specifications/, which specify how to format one of the arguments
-- to 'printf' in the output string. Unlike C @printf(3)@, the formatting
-- is driven by the argument type; formatting is type specific. The
-- types formatted by 'printf' \"out of the box\" are:
--
--   * 'Integral' types, including 'Char': 'printf' makes no real distinction
--
--   * 'String'
--
--   * 'RealFloat' types
--
-- 'printf' is also extensible to support other types: see below.
--
-- A conversion specification begins with the
-- character @%@, followed by one or more of the following flags:
--
-- >    -      left adjust (default is right adjust)
-- >    +      always use a sign (+ or -) for signed conversions
-- >    space  leading space for positive numbers in signed conversions
-- >    0      pad with zeros rather than spaces
-- >    #      use an \"alternate form\": see below
--
-- When both flags are given, @-@ overrides @0@ and @+@ overrides space.
--
-- The \"alternate form\" for unsigned radix conversions is
-- as in C @printf(3)@:
--
-- >    %o     prefix with a leading 0 if not already zero
-- >    %x     prefix with a leading 0x
-- >    %X     prefix with a leading 0X
--
-- An alternate form for 'RealFloat' conversions is silently
-- ignored: the differences in floating point formatting
-- from C @printf(3)@ noted below make it difficult to
-- do anything sensible.
--
-- Any flags are followed optionally by a field width:
--
-- >    num    field width
-- >    *      as num, but taken from argument list
--
-- followed optionally by a precision:
--
-- >    .num   precision (number of decimal places)
-- >    .      same as .0
-- >    .*     as num, but taken from argument list
--
-- followed optionally for Integral types by a width
-- specifier; the only use of this specifier being to set
-- the implicit size of the operand for conversion of
-- a negative operand to unsigned:
--
-- >    hh     Int8
-- >    h      Int16
-- >    l      Int32
-- >    ll     Int64
-- >    L      Int64
--
-- and finally, ending with a format character:
--
-- >    c      character               Integral
-- >    d      decimal                 Integral
-- >    o      octal                   Integral
-- >    x      hexadecimal             Integral
-- >    X      hexadecimal             Integral
-- >    u      unsigned decimal        Integral
-- >    f      floating point          RealFloat
-- >    F      floating point          RealFloat
-- >    g      general format float    RealFloat
-- >    G      general format float    RealFloat
-- >    e      exponent format float   RealFloat
-- >    E      exponent format float   RealFloat
-- >    s      string                  String
--
-- Mismatch between the argument types and the format
-- string, as well as any other syntactic or semantic errors
-- in the format string, will cause an exception to be
-- thrown at runtime.
--
-- Note that the formatting for 'RealFloat' types is
-- currently quite different from that of C @printf(3)@,
-- conforming instead to 'Numeric.showEFloat',
-- 'Numeric.showFFloat' and 'Numeric.showGFloat'. This is
-- hard to fix, and the fixed versions would format in a
-- backward-incompatible way.
--
-- Examples:
--
-- >   > printf "%d\n" (23::Int)
-- >   23
-- >   > printf "%s %s\n" "Hello" "World"
-- >   Hello World
-- >   > printf "%.2f\n" pi
-- >   3.14
--
printf :: (PrintfType r) => String -> r
printf fmts = spr fmts []

-- | Similar to 'printf', except that output is via the specified
-- 'Handle'.  The return type is restricted to @('IO' ())@.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmts = hspr hdl fmts []

-- | The 'PrintfType' class provides the variable argument magic for
-- 'printf'.
class PrintfType t where
    spr :: String -> [UPrintf] -> t

-- | The 'HPrintfType' class provides the variable argument magic for
-- 'hPrintf'.
class HPrintfType t where
    hspr :: Handle -> String -> [UPrintf] -> t

{-
   This is not allowed in Haskell 98, because String
   is a concrete type:

     instance PrintfType String where
       spr fmt args = uprintf fmt (reverse args)

   I have decided I don't care here, and am going to use
   FlexibleInstances for clarity. This also allows getting the
   type of printf and hprintf to be IO ().
-}

instance PrintfType String where
    spr fmts args = uprintf fmts (reverse args)

instance PrintfType (IO ()) where
    spr fmts args = putStr (uprintf fmts (reverse args))

instance HPrintfType (IO ()) where
    hspr hdl fmts args = hPutStr hdl (uprintf fmts (reverse args))

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts
                             ((parseFormat a, toField a) : args)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmts args = \ a -> hspr hdl fmts
                                  ((parseFormat a, toField a) : args)

-- | Whether to left-adjust or zero-pad a field. These are
-- mutually exclusive, with 'LeftAdjust' taking precedence.
data FormatAdjustment = LeftAdjust | ZeroPad

-- | How to handle the sign of a numeric field.  These are
-- mutually exclusive, with 'SignPlus' taking precedence.
data FormatSign = SignPlus | SignSpace

-- | Description of field formatting for 'toField'. See UNIX `printf`(3)
-- for a description of how field formatting works.
data FieldFormat = FieldFormat {
  fmtWidth :: Maybe Int,       -- ^ Total width of the field.
  fmtPrecision :: Maybe Int,   -- ^ Secondary field width specifier.
  fmtAdjust :: Maybe FormatAdjustment,  -- ^ Kind of filling or padding
                                        --   to be done.
  fmtSign :: Maybe FormatSign, -- ^ Whether to insist on a
                               -- plus sign for positive
                               -- numbers.
  fmtAlternate :: Bool,        -- ^ Indicates an "alternate
                               -- format".  See printf(3)
                               -- for the details, which
                               -- vary by argument spec.
  fmtModifiers :: String,      -- ^ Characters that appeared
                               -- immediately to the left of
                               -- 'fmtChar' in the format
                               -- and were accepted by the
                               -- type's 'parseFormat'.
                               -- Normally the empty string.
  fmtChar :: Char              -- ^ The format character
                               -- 'printf' was invoked
                               -- with. 'toField' should
                               -- fail unless this character
                               -- matches the type. It is
                               -- normal to handle many
                               -- different format
                               -- characters for a single
                               -- type.
  }

-- | The \"format parser\" walks over argument-type-specific
-- modifier characters to find the primary format character.
-- This is the type of its result.
data FormatParse = FormatParse {
  fpModifiers :: String,   -- ^ Any modifiers found.
  fpChar :: Char,          -- ^ Primary format character.
  fpRest :: String         -- ^ Rest of the format string.
  }

-- Contains the "modifier letters" that can precede an
-- integer type.
intModifierMap :: Map String Integer
intModifierMap = fromList [
  ("hh", toInteger (minBound :: Int8)),
  ("h", toInteger (minBound :: Int16)),
  ("l", toInteger (minBound :: Int32)),
  ("ll", toInteger (minBound :: Int64)),
  ("L", toInteger (minBound :: Int64)) ]
-- ("q", minBound :: Int64)
-- ("j", minBound :: "IntMax or UIntMax")
-- ("z", minBound :: "Size_T or SSize_T")
-- ("t", minBound :: "PtrDiff_T")

parseIntFormat :: Integral a => a -> String -> FormatParse
parseIntFormat _ s =
  case foldrWithKey matchPrefix Nothing intModifierMap of
    Just m -> m
    Nothing ->
      case s of
        c : cs -> FormatParse "" c cs
        "" -> fmterr
  where
    matchPrefix p _ m@(Just (FormatParse p0 _ _))
      | length p0 >= length p = m
      | otherwise = case getFormat p of
          Nothing -> m
          Just fp -> Just fp
    matchPrefix p _ Nothing =
      getFormat p
    getFormat p =
      stripPrefix p s >>= fp
      where
        fp (c : cs) = Just $ FormatParse p c cs
        fp "" = fmterr

-- | This is the type of a field formatter reified over its
-- argument.
type FieldFormatter = FieldFormat -> ShowS

-- | Type of a function that will parse modifier characters
-- from the format string.
type ModifierParser = String -> FormatParse

-- | Typeclass of 'printf'-formattable values. The 'toField' method
-- takes a value and a field format descriptor and either fails due
-- to a bad descriptor or produces a 'ShowS' as the result. The
-- default 'parseFormat' expects no modifiers: this is the normal
-- case. Minimal instance: 'toField'.
class PrintfArg a where
    toField :: a -> FieldFormatter
    parseFormat :: a -> ModifierParser
    parseFormat _ (c : cs) = FormatParse "" c cs
    parseFormat _ "" = fmterr

instance PrintfArg Int where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Int8 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Int16 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Int32 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Int64 where
    toField = formatInt
    parseFormat = parseIntFormat

#ifndef __NHC__
instance PrintfArg Word where
    toField = formatInt
    parseFormat = parseIntFormat
#endif

instance PrintfArg Word8 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Word16 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Word32 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Word64 where
    toField = formatInt
    parseFormat = parseIntFormat

instance PrintfArg Integer where
    toField = formatInteger
    parseFormat = parseIntFormat

instance PrintfArg Float where
    toField = formatRealFloat

instance PrintfArg Double where
    toField = formatRealFloat

instance PrintfArg Char where
    toField = formatChar
    parseFormat _ fmt = parseIntFormat (undefined :: Int) fmt

instance PrintfArg String where
    toField = formatString

-- | Formatter for 'Char' values.
formatChar :: Char -> FieldFormatter
formatChar x ufmt =
  formatIntegral (Just 0) (toInteger $ ord x) ufmt

unprec :: FieldFormat -> Int
unprec ufmt =
  case fmtPrecision ufmt of
    Just p -> p
    Nothing -> -1

-- | Formatter for 'String' values.
formatString :: String -> FieldFormatter
formatString x ufmt =
  case fmtChar ufmt of
    's' -> (adjust ufmt ("", trunc (unprec ufmt)) ++)
           where
             trunc n = if n >= 0 then take n x else x
    c   -> badfmterr c

fixupMods :: FieldFormat -> Maybe Integer -> Maybe Integer
fixupMods ufmt m =
  let mods = fmtModifiers ufmt in
  case mods of
    "" -> m
    _ -> case M.lookup mods intModifierMap of
      Just m0 -> Just m0
      Nothing -> perror "internal error: unknown format modifier"

-- | Formatter for 'Int' values.
formatInt :: (Integral a, Bounded a) => a -> FieldFormatter
formatInt x ufmt =
  let m = fixupMods ufmt (Just $ toInteger $ minBound `asTypeOf` x) in
  formatIntegral m (toInteger x) ufmt

-- | Formatter for 'Integer' values.
formatInteger :: Integer -> FieldFormatter
formatInteger x ufmt =
  let m = fixupMods ufmt Nothing in
  formatIntegral m x ufmt

-- All formatting for integral types is handled
-- consistently.  The only difference is between Integer and
-- bounded types; this difference is handled by the 'm'
-- argument containing the lower bound.
formatIntegral :: Maybe Integer -> Integer -> FieldFormatter
formatIntegral m x ufmt =
  let prec = unprec ufmt in
  case fmtChar ufmt of
    'd' -> (adjustSigned ufmt (fmti prec x) ++)
    'i' -> (adjustSigned ufmt (fmti prec x) ++)
    'x' -> (adjust ufmt (altprefix, fmtu 16 prec m x) ++)
           where
             altprefix  = case fmtAlternate ufmt of
               True -> "0x"
               False -> ""
    'X' -> (adjust ufmt (altprefix, map toUpper $ fmtu 16 prec m x) ++)
           where
             altprefix  = case fmtAlternate ufmt of
               True -> "0X"
               False -> ""
    'o' -> (adjust ufmt (altprefix, fmtu 8 prec m x) ++)
           where
             altprefix  = case x /= 0 && fmtAlternate ufmt of
               True -> "0"
               False -> ""
    'u' -> (adjust ufmt ("", fmtu 10 prec m x) ++)
    'c' | x >= fromIntegral (ord (minBound :: Char)) &&
          x <= fromIntegral (ord (maxBound :: Char)) &&
          fmtPrecision ufmt == Nothing &&
          fmtModifiers ufmt == "" ->
            formatString [chr $ fromIntegral x] (ufmt { fmtChar = 's' })
    'c' -> perror "illegal char conversion"
    c   -> badfmterr c

-- | Formatter for 'RealFloat' values.
formatRealFloat :: RealFloat a => a -> FieldFormatter
formatRealFloat x ufmt =
  let c = fmtChar ufmt
      prec = fmtPrecision ufmt
  in
   case c of
     'e' -> (adjustSigned ufmt (dfmt c prec x) ++)
     'E' -> (adjustSigned ufmt (dfmt c prec x) ++)
     'f' -> (adjustSigned ufmt (dfmt c prec x) ++)
     'F' -> (adjustSigned ufmt (dfmt c prec x) ++)
     'g' -> (adjustSigned ufmt (dfmt c prec x) ++)
     'G' -> (adjustSigned ufmt (dfmt c prec x) ++)
     _   -> badfmterr c

-- This is the type carried around for arguments in
-- the varargs code.
type UPrintf = (ModifierParser, FieldFormatter)

-- Given a format string and a list of formatting functions
-- (the actual field value having already been baked into
-- each of these functions before delivery), return the
-- actual formatted text string.
uprintf :: String -> [UPrintf] -> String
uprintf s us = uprintfs s us ""

-- This function does the actual work, producing a ShowS
-- instead of a string, for future expansion and for
-- misguided efficiency.
uprintfs :: String -> [UPrintf] -> ShowS
uprintfs ""       []       = id
uprintfs ""       (_:_)    = fmterr
uprintfs ('%':'%':cs) us   = ('%' :) . uprintfs cs us
uprintfs ('%':_)  []       = argerr
uprintfs ('%':cs) us@(_:_) = fmt cs us
uprintfs (c:cs)   us       = (c :) . uprintfs cs us

-- Given a suffix of the format string starting just after
-- the percent sign, and the list of remaining unprocessed
-- arguments in the form described above, format the portion
-- of the output described by this field description, and
-- then continue with 'uprintfs'.
fmt :: String -> [UPrintf] -> ShowS
fmt cs0 us0 =
  case getSpecs False False Nothing False cs0 us0 of
    (_, _, []) -> argerr
    (ufmt, cs, (_, u) : us) -> u ufmt . uprintfs cs us

-- Given field formatting information, and a tuple
-- consisting of a prefix (for example, a minus sign) that
-- is supposed to go before the argument value and a string
-- representing the value, return the properly padded and
-- formatted result.
adjust :: FieldFormat -> (String, String) -> String
adjust ufmt (pre, str) =
  let naturalWidth = length pre + length str
      zero = case fmtAdjust ufmt of
        Just ZeroPad -> True
        _ -> False
      left = case fmtAdjust ufmt of
        Just LeftAdjust -> True
        _ -> False
      fill = case fmtWidth ufmt of
        Just width | naturalWidth < width ->
          let fillchar = if zero then '0' else ' ' in
          replicate (width - naturalWidth) fillchar
        _ -> ""
  in
   if left
   then pre ++ str ++ fill
   else if zero
        then pre ++ fill ++ str
        else fill ++ pre ++ str

-- For positive numbers with an explicit sign field ("+" or
-- " "), adjust accordingly.
adjustSigned :: FieldFormat -> (String, String) -> String
adjustSigned ufmt@(FieldFormat {fmtSign = Just SignPlus}) ("", str) =
  adjust ufmt ("+", str)
adjustSigned ufmt@(FieldFormat {fmtSign = Just SignSpace}) ("", str) =
  adjust ufmt (" ", str)
adjustSigned ufmt ps =
  adjust ufmt ps

-- Format a signed integer in the "default" fashion.
-- This will be subjected to adjust subsequently.
fmti :: Int -> Integer -> (String, String)
fmti prec i
  | i < 0 = ("-", integral_prec prec (show (-i)))
  | otherwise = ("", integral_prec prec (show i))

-- Format an unsigned integer in the "default" fashion.
-- This will be subjected to adjust subsequently.  The 'b'
-- argument is the base, and the '(Just m)' argument is the
-- implicit lower-bound size of the operand for conversion
-- from signed to unsigned. Thus, this function will refuse
-- to convert an unbounded negative integer to an unsigned
-- string.
fmtu :: Integer -> Int -> Maybe Integer -> Integer -> String
fmtu b prec _ i | i >= 0 =
  integral_prec prec (itosb b i)
fmtu b prec (Just m) i =
  integral_prec prec (itosb b (-2 * m + i))
fmtu _ _ _ _ =
  baderr

-- This is used by 'fmtu' and 'fmti' to zero-pad an
-- int-string to a required precision.
integral_prec :: Int -> String -> String
integral_prec prec integral =
  replicate (prec - length integral) '0' ++ integral

itosb :: Integer -> Integer -> String
itosb b n =
        if n < b then
            [intToDigit $ fromInteger n]
        else
            let (q, r) = quotRem n b in
            itosb b q ++ [intToDigit $ fromInteger r]

stoi :: String -> (Int, String)
stoi cs =
  let (as, cs') = span isDigit cs in
  case as of
    "" -> (0, cs')
    _ -> (read as, cs')

adjustment :: Bool -> Bool -> Maybe FormatAdjustment
adjustment False False = Nothing
adjustment True False = Just LeftAdjust
adjustment False True = Just ZeroPad
adjustment True True = Just LeftAdjust

getSpecs :: Bool -> Bool -> Maybe FormatSign -> Bool -> String -> [UPrintf]
         -> (FieldFormat, String, [UPrintf])
getSpecs _ z s a ('-' : cs0) us = getSpecs True z s a cs0 us
getSpecs l z _ a ('+' : cs0) us = getSpecs l z (Just SignPlus) a cs0 us
getSpecs l z s a (' ' : cs0) us = 
  getSpecs l z ss a cs0 us
  where
    ss = case s of
      Just SignPlus -> Just SignPlus
      _ -> Just SignSpace
getSpecs l _ s a ('0' : cs0) us = getSpecs l True s a cs0 us
getSpecs l z s _ ('#' : cs0) us = getSpecs l z s True cs0 us
getSpecs l z s a ('*' : cs0) us =
  let (us', n) = getStar us
      ((p, cs''), us'') = case cs0 of
        '.':'*':r ->
          let (us''', p') = getStar us' in ((Just p', r), us''')
        '.':r -> 
          let (p', r') = stoi r in ((Just p', r'), us')
        _ ->
          ((Nothing, cs0), us')
      FormatParse ms c cs =
        case us'' of
          (ufmt, _) : _ -> ufmt cs''
          [] -> argerr
  in
   (FieldFormat {
       fmtWidth = Just n,
       fmtPrecision = p,
       fmtAdjust = adjustment l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us'')
getSpecs l z s a ('.' : cs0) us =
  let ((p, cs'), us') = case cs0 of
        '*':cs'' -> let (us'', p') = getStar us in ((p', cs''), us'')
        _ ->        (stoi cs0, us)
      FormatParse ms c cs =
        case us' of
          (ufmt, _) : _ -> ufmt cs'
          [] -> argerr
  in
   (FieldFormat {
       fmtWidth = Nothing,
       fmtPrecision = Just p,
       fmtAdjust = adjustment l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us')
getSpecs l z s a cs0@(c0 : _) us | isDigit c0 =
  let (n, cs') = stoi cs0
      ((p, cs''), us') = case cs' of
        '.' : '*' : r ->
          let (us'', p') = getStar us in ((Just p', r), us'')
        '.' : r ->
          let (p', r') = stoi r in ((Just p', r'), us)
        _ ->
          ((Nothing, cs'), us)
      FormatParse ms c cs =
        case us' of
          (ufmt, _) : _ -> ufmt cs''
          [] -> argerr
  in
   (FieldFormat {
       fmtWidth = Just n,
       fmtPrecision = p,
       fmtAdjust = adjustment l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us')
getSpecs l z s a cs0@(_ : _) us =
  let FormatParse ms c cs =
        case us of
          (ufmt, _) : _ -> ufmt cs0
          [] -> argerr
  in
   (FieldFormat {
       fmtWidth = Nothing,
       fmtPrecision = Nothing,
       fmtAdjust = adjustment l z,
       fmtSign = s,
       fmtAlternate = a,
       fmtModifiers = ms,
       fmtChar = c}, cs, us)
getSpecs _ _ _ _ ""       _  =
  fmterr

getStar :: [UPrintf] -> ([UPrintf], Int)
getStar us =
  let ufmt = FieldFormat {
        fmtWidth = Nothing,
        fmtPrecision = Nothing,
        fmtAdjust = Nothing,
        fmtSign = Nothing,
        fmtAlternate = False,
        fmtModifiers = "",
        fmtChar = 'd' } in
  case us of
    [] -> argerr
    (_, nu) : us' -> (us', read (nu ufmt ""))

dfmt :: (RealFloat a) => Char -> Maybe Int -> a -> (String, String)
dfmt c p d =
  let caseConvert = if isUpper c then map toUpper else id
      showFunction = case toLower c of
        'e' -> showEFloat
        'f' -> showFFloat
        'g' -> showGFloat
        _   -> error "Printf.dfmt: impossible"
      result = caseConvert $ showFunction p d ""
  in
   case result of
     '-' : cs -> ("-", cs)
     cs       -> ("" , cs)

perror :: String -> a
perror s = error ("Printf.printf: "++s)

badfmterr :: Char -> a
badfmterr c =
  perror $ "bad formatting char " ++ show c

fmterr, argerr, baderr :: a
fmterr = perror "formatting string ended prematurely"
argerr = perror "argument list ended prematurely"
baderr = perror "bad argument"
