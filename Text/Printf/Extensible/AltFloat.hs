{-# LANGUAGE CPP, FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{- Thanks to http://stackoverflow.com/questions/11171325/ for the above. -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Printf.Extensible.AltFloat
-- Copyright   :  © 2013 The University of Glasgow, Bart Massey and 
--                Lennart Augusstson
-- License     :  BSD-style (see the file LICENSE in this distribution)
--
-- Maintainer  :  bart@cs.pdx.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- This code is a copy of 'Numeric.formatRealFloat' and some
-- code from 'GHC.Float' that has been altered to show
-- floats in an "alternate" format compatible with the "#"
-- field modifier of 'Text.printf'.
-----------------------------------------------------------------------------

module Text.Printf.Extensible.AltFloat (
  FFFormat(..), formatRealFloat, 
  showEFloat, showFFloat, showGFloat
) where

import Data.Char (intToDigit)
import GHC.Float (FFFormat(..), roundTo)
import Numeric (floatToDigits) 
  
-- Controlling the format and precision of floats.

{-# SPECIALIZE showEFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}
-- | Show a signed 'RealFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'showEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showEFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat d x =  showString (formatRealFloat FFExponent d x)

{-# SPECIALIZE showFFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}
-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'showFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showFFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat d x =  showString (formatRealFloat FFFixed d x)

{-# SPECIALIZE showGFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}
-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies 
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'showGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showGFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat d x =  showString (formatRealFloat FFGeneric d x)


-- | Given a 'FFFormat', maybe a number of digits to print
-- after the decimal point, and a 'RealFloat', format the
-- number accordingly and return the resulting string.
-- The mantissa of the result is produced according to the
-- \'#\' format of 'Text.Printf.Extended.printf'.
formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat fmt decs x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = '-':doFmt fmt (floatToDigits (toInteger base) (-x))
   | otherwise                 = doFmt fmt (floatToDigits (toInteger base) x)
 where
  base = 10

  doFmt format (is, e) =
    let ds = map intToDigit is in
    case format of
     FFGeneric ->
      doFmt (if e < 0 || e > 7 then FFExponent else FFFixed)
            (is,e)
     FFExponent ->
      case decs of
       Nothing ->
        let show_e' = show (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> d : ".0e" ++ show_e'
          (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
          []      -> error "formatRealFloat/doFmt/FFExponent: []"
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
         _ ->
          let
           (ei,is') = roundTo base (dec'+1) is
           (d:ds') = map intToDigit (if ei > 0 then init is' else is')
          in
          d:'.':ds' ++ 'e':show (e-1+ei)
     FFFixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                f n s    ""  = f (n-1) ('0':s) ""
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo base (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map intToDigit is')
         in
         mk0 ls ++ (if null rs then "" else '.':rs)
        else
         let
          (ei,is') = roundTo base dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' then "" else '.':ds')
