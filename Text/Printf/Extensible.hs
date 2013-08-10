{-# LANGUAGE CPP, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{- Thanks to http://stackoverflow.com/questions/11171325/ for the above. -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf.Extensible
-- Copyright   :  (c) Lennart Augustsson and Bart Massey 2013
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  lennart@augustsson.net
-- Stability   :  provisional
-- Portability :  portable
--
-- A C printf like formatter. This version has been extended by
-- Bart Massey as per the recommendations of John Meacham and
-- Simon Marlow 
-- <http://comments.gmane.org/gmane.comp.lang.haskell.libraries/4726> 
-- to support extensible formatting for new datatypes.
-- It has also been extended to support most printf(3) syntax.
--
-----------------------------------------------------------------------------

module Text.Printf.Extensible (
   printf, hPrintf,
   PrintfType, HPrintfType, 
   formatChar, formatString, formatInt,
   formatInteger, formatRealFloat,
   FieldFormat(..), PrintfArg(..)
) where

import Prelude
import Data.Char
import Data.Int
import Data.Word
import Numeric(showEFloat, showFFloat, showGFloat)
import System.IO

-------------------

-- | Format a variable number of arguments with the C-style formatting string.
-- The return value is either 'String' or @('IO' a)@.
--
-- The format string consists of ordinary characters and /conversion
-- specifications/, which specify how to format one of the arguments
-- to printf in the output string.  A conversion specification begins with the
-- character @%@, followed by one or more of the following flags:
--
-- >    -      left adjust (default is right adjust)
-- >    +      always use a sign (+ or -) for signed conversions
-- >    0      pad with zeroes rather than spaces
--
-- followed optionally by a field width:
-- 
-- >    num    field width
-- >    *      as num, but taken from argument list
--
-- followed optionally by a precision:
--
-- >    .num   precision (number of decimal places)
--
-- and finally, a format character:
--
-- >    c      character               Char, Int, Integer, ...
-- >    d      decimal                 Char, Int, Integer, ...
-- >    o      octal                   Char, Int, Integer, ...
-- >    x      hexadecimal             Char, Int, Integer, ...
-- >    X      hexadecimal             Char, Int, Integer, ...
-- >    u      unsigned decimal        Char, Int, Integer, ...
-- >    f      floating point          Float, Double
-- >    g      general format float    Float, Double
-- >    G      general format float    Float, Double
-- >    e      exponent format float   Float, Double
-- >    E      exponent format float   Float, Double
-- >    s      string                  String
--
-- Mismatch between the argument types and the format string will cause
-- an exception to be thrown at runtime.
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
-- 'Handle'.  The return type is restricted to @('IO' a)@.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf hdl fmts = hspr hdl fmts []

-- |The 'PrintfType' class provides the variable argument magic for
-- 'printf'.  Its implementation is intentionally not visible from
-- this module. If you attempt to pass an argument of a type which
-- is not an instance of this class to 'printf' or 'hPrintf', then
-- the compiler will report it as a missing instance of 'PrintfArg'.
-- (All 'PrintfArg' instances are 'PrintfType' instances.)
class PrintfType t where
    spr :: String -> [UPrintf] -> t

-- | The 'HPrintfType' class provides the variable argument magic for
-- 'hPrintf'.  Its implementation is intentionally not visible from
-- this module.
class HPrintfType t where
    hspr :: Handle -> String -> [UPrintf] -> t

{- 
   These are not allowed in Haskell 98:

     instance PrintfType String where
       spr fmt args = uprintf fmt (reverse args)

   I have decided I don't care here, and am
   going to use [Char] and FlexibleInstances
   for clarity. This also allows getting the
   type of printf and hprintf to be IO (), which
   is important now that GHC gives warnings on
   ignored returns.
-}

instance PrintfType [Char] where
    spr fmts args = uprintf fmts (reverse args)

instance PrintfType (IO ()) where
    spr fmts args = do
        putStr (uprintf fmts (reverse args))
        return (error "PrintfType (IO a): result should not be used.")

instance HPrintfType (IO ()) where
    hspr hdl fmts args = do
        hPutStr hdl (uprintf fmts (reverse args))
        return (error "HPrintfType (IO a): result should not be used.")

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts (toField a : args)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
    hspr hdl fmts args = \ a -> hspr hdl fmts (toField a : args)

-- | Whether to left-adjust or zero-pad a field. These are
-- mutually exclusive. 
data Adjustment = LeftAdjust | ZeroPad

data Sign = SignPlus | SignSpace | SignNothing

-- | Description of field formatting for 'toField'. See UNIX `printf`(3)
-- for a description of how field formatting works.
data FieldFormat = FieldFormat {
  fmtWidth :: Maybe Int,          -- ^ Total width of the field.
  fmtPrecision :: Maybe Int,      -- ^ A secondary field width specifier.
  fmtAdjust :: Maybe Adjustment,  -- ^ Kind of filling or padding to be done.
  fmtSign :: Sign,                -- ^ Whether to insist on a plus sign for
                                    --   positive numbers.
  fmtAlternate :: Bool,           -- ^ Indicates an "alternate format".
                                    --   See printf(3) for the details,
                                    --   which vary by argument spec.
  fmtChar :: Char            -- ^ The format character 'printf' was
                                    --   invoked with. 'toField' should
                                    --   fail unless this character matches
                                    --   the type. It is normal to handle
                                    --   many different format characters for
                                    --   a single type.
  }

-- | Typeclass of 'printf'-formattable values. The 'toField' method
-- takes a value and a field format descriptor and either fails due
-- to a bad descriptor or produces a 'ShowS' as the result.
class PrintfArg a where
    toField :: a -> FieldFormat -> ShowS

instance PrintfArg Char where
    toField = formatChar

instance PrintfArg [Char] where
    toField = formatString

instance PrintfArg Int where
    toField = formatInt

instance PrintfArg Int8 where
    toField = formatInt

instance PrintfArg Int16 where
    toField = formatInt

instance PrintfArg Int32 where
    toField = formatInt

instance PrintfArg Int64 where
    toField = formatInt

#ifndef __NHC__
instance PrintfArg Word where
    toField = formatInt
#endif

instance PrintfArg Word8 where
    toField = formatInt

instance PrintfArg Word16 where
    toField = formatInt

instance PrintfArg Word32 where
    toField = formatInt

instance PrintfArg Word64 where
    toField = formatInt

instance PrintfArg Integer where
    toField = formatInteger

instance PrintfArg Float where
    toField = formatRealFloat

instance PrintfArg Double where
    toField = formatRealFloat

-- | Formatter for 'Char' values.
formatChar :: Char -> FieldFormat -> ShowS
formatChar x ufmt =
  case fmtChar ufmt of
    'c' -> (adjust ufmt ("", [x]) ++)
    _   -> formatInteger (toInteger $ ord x) ufmt

unprec :: FieldFormat -> Int
unprec ufmt = 
  case fmtPrecision ufmt of
    Just p -> p
    Nothing -> -1

-- | Formatter for 'String' values.
formatString :: String -> FieldFormat -> ShowS
formatString x ufmt =
  case fmtChar ufmt of
    's' -> (adjust ufmt ("", tostr (unprec ufmt) x) ++)
    c   -> badfmterr c

-- | Formatter for 'Int' values.
formatInt :: (Integral a, Bounded a) => a -> FieldFormat -> ShowS
formatInt x ufmt =
  let m = toInteger $ minBound `asTypeOf` x in
  formatIntegral (Just m) (toInteger x) ufmt
  
-- | Formatter for 'Integer' values.
formatInteger :: Integer -> FieldFormat -> ShowS
formatInteger x ufmt =
  formatIntegral Nothing x ufmt

formatIntegral :: Maybe Integer -> Integer -> FieldFormat -> ShowS
formatIntegral m x ufmt =
  let prec = unprec ufmt in
  case fmtChar ufmt of
    'd' -> (adjustSigned ufmt (fmti prec x) ++)
    'i' -> (adjustSigned ufmt (fmti prec x) ++)
    'x' -> (adjust ufmt ("", fmtu 16 prec m x) ++)
    'X' -> (adjust ufmt ("", map toUpper $ fmtu 16 prec m x) ++)
    'o' -> (adjust ufmt ("", fmtu 8 prec m x) ++)
    'u' -> (adjust ufmt ("", fmtu 10 prec m x) ++)
    'c' | x >= fromIntegral (ord (minBound :: Char)) && 
          x <= fromIntegral (ord (maxBound :: Char)) ->
            formatChar (chr $ fromIntegral x) ufmt
    'c' -> perror "illegal int to char conversion"
    c   -> badfmterr c

-- | Formatter for 'RealFloat' values.
formatRealFloat :: RealFloat a => a -> FieldFormat -> ShowS
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

type UPrintf = FieldFormat -> ShowS

uprintf :: String -> [UPrintf] -> String
uprintf s us = uprintfs s us ""

uprintfs :: String -> [UPrintf] -> ShowS
uprintfs ""       []       = id
uprintfs ""       (_:_)    = fmterr
uprintfs ('%':'%':cs) us   = ('%' :) . uprintfs cs us
uprintfs ('%':_)  []       = argerr
uprintfs ('%':cs) us@(_:_) = fmt cs us
uprintfs (c:cs)   us       = (c :) . uprintfs cs us

fmt :: String -> [UPrintf] -> ShowS
fmt cs0 us0 =
  fmt' $ getSpecs False False SignNothing False cs0 us0
  where
    fmt' (_, _, []) = argerr
    fmt' (ufmt, cs, u : us) = u ufmt . uprintfs cs us

adjust :: FieldFormat -> (String, String) -> String
adjust ufmt (pre, str) = 
  let lstr = length str
      lpre = length pre
      zero = case fmtAdjust ufmt of
        Just ZeroPad -> True
        _ -> False
      left = case fmtAdjust ufmt of
        Just LeftAdjust -> True
        _ -> False
      fill = case fmtWidth ufmt of
        Just width | lstr + lpre < width ->
          let fillchar = if zero then '0' else ' ' in
          replicate (width - (lstr + lpre)) fillchar
        _ -> ""
  in
   if left
   then pre ++ str ++ fill
   else if zero
        then pre ++ fill ++ str
        else fill ++ pre ++ str

adjustSigned :: FieldFormat -> (String, String) -> String
adjustSigned ufmt@(FieldFormat {fmtSign = SignPlus}) ("", str) = 
  adjust ufmt ("+", str)
adjustSigned ufmt@(FieldFormat {fmtSign = SignSpace}) ("", str) = 
  adjust ufmt (" ", str)
adjustSigned ufmt ps = 
  adjust ufmt ps

fmti :: Int -> Integer -> (String, String)
fmti prec i
  | i < 0 = ("-", integral_prec prec (show (-i))) 
  | otherwise = ("", integral_prec prec (show i))

fmtu :: Integer -> Int -> Maybe Integer -> Integer -> String
fmtu b prec _ i | i > 0 =
  integral_prec prec (itosb b i)
fmtu b prec (Just m) i =
  integral_prec prec (itosb b (-2 * m + i))
fmtu _ _ _ _ =
  baderr

integral_prec :: Int -> String -> String
integral_prec prec integral = 
  replicate (prec - (length integral)) '0' ++ integral

tostr :: Int -> String -> String
tostr n s = if n >= 0 then take n s else s

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

adjustment :: Bool -> Bool -> Maybe Adjustment
adjustment False False = Nothing
adjustment True False = Just LeftAdjust
adjustment False True = Just ZeroPad
adjustment True True = Just LeftAdjust

-- XXX need to ignore length specifiers "hlLqjzt"
getSpecs :: Bool -> Bool -> Sign -> Bool -> String -> [UPrintf] 
         -> (FieldFormat, String, [UPrintf])
getSpecs _ z s a ('-' : cs0) us = getSpecs True z s a cs0 us
getSpecs l z _ a ('+' : cs0) us = getSpecs l z SignPlus a cs0 us
getSpecs l z _ a (' ' : cs0) us = getSpecs l z SignSpace a cs0 us
getSpecs l _ s a ('0' : cs0) us = getSpecs l True s a cs0 us
getSpecs l z s _ ('#' : cs0) us = getSpecs l z s True cs0 us
getSpecs l z s a ('*' : cs0) us =
  let (us', n) = getStar us
      ((p, c : cs), us'') = case cs0 of
        '.':'*':r -> 
          let (us''', p') = getStar us' in ((p', r), us''')
        '.':r -> 
          (stoi r, us')
        _ -> 
          ((-1, cs0), us')
  in
   (FieldFormat {
       fmtWidth = Just n, 
       fmtPrecision = Just p, 
       fmtAdjust = adjustment l z, 
       fmtSign = s,
       fmtAlternate = a,
       fmtChar = c}, cs, us'')
getSpecs l z s a ('.' : cs0) us =
  let ((p, c : cs), us') = case cs0 of
        '*':cs'' -> let (us'', p') = getStar us in ((p', cs''), us'')
        _ ->        (stoi cs0, us)
  in  
   (FieldFormat {
       fmtWidth = Nothing, 
       fmtPrecision = Just p, 
       fmtAdjust = adjustment l z, 
       fmtSign = s,
       fmtAlternate = a,
       fmtChar = c}, cs, us')
getSpecs l z s a cs0@(c0 : _) us | isDigit c0 =
  let (n, cs') = stoi cs0
      ((p, c : cs), us') = case cs' of
        '.' : '*' : r ->
          let (us'', p') = getStar us in ((p', r), us'')
        '.' : r -> 
          (stoi r, us)
        _ -> 
          ((-1, cs'), us)
  in 
   (FieldFormat {
       fmtWidth = Just n, 
       fmtPrecision = Just p, 
       fmtAdjust = adjustment l z, 
       fmtSign = s,
       fmtAlternate = a,
       fmtChar = c}, cs, us')
getSpecs l z s a (c : cs) us = 
  (FieldFormat {
      fmtWidth = Nothing, 
      fmtPrecision = Nothing, 
      fmtAdjust = adjustment l z, 
      fmtSign = s,
      fmtAlternate = a,
      fmtChar = c}, cs, us)
getSpecs _ _ _ _ ""       _  =
  fmterr

getStar :: [UPrintf] -> ([UPrintf], Int)
getStar us =
  let ufmt = FieldFormat {
        fmtWidth = Nothing,
        fmtPrecision = Nothing,
        fmtAdjust = Nothing,
        fmtSign = SignNothing,
        fmtAlternate = False,
        fmtChar = 'd' } in
  case us of
    [] -> argerr
    nu : us' -> (us', read (nu ufmt ""))

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
