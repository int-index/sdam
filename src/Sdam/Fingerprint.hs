{-# LANGUAGE BangPatterns, TypeApplications #-}

module Sdam.Fingerprint
  ( Fingerprint(..),
    fingerprintString,
    fingerprintFingerprints,
    fingerprintToString,
    stringToFingerprint,
    isFingerprintChar
  ) where

import Data.Word (Word64)
import qualified Data.List as List
import Numeric.Natural (Natural)
import Control.Monad ((<=<))
import Data.Traversable (for)
import Data.Maybe (listToMaybe)
import GHC.Fingerprint

fingerprintToNatural :: Fingerprint -> Natural
fingerprintToNatural (Fingerprint a b) =
  fromIntegral a * word64_offset + fromIntegral b

naturalToFingerprint :: Natural -> Maybe Fingerprint
naturalToFingerprint n =
  let
    (n1, b') = divMod n word64_offset
    (n2, a') = divMod n1 word64_offset
    a = fromIntegral a'
    b = fromIntegral b'
  in
    if n2 /= 0 then Nothing else Just (Fingerprint a b)

word64_offset :: Natural
word64_offset = fromIntegral (maxBound :: Word64) + 1

alphabet :: [Char]
alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

isFingerprintChar :: Char -> Bool
isFingerprintChar c = c `elem` alphabet

toRevDigits :: Natural -> Natural -> [Int]
toRevDigits _    0 = List.repeat 0
toRevDigits base n = fromIntegral r : toRevDigits base q
  where
    (q, r) = quotRem n base

naturalToString :: Natural -> String
naturalToString n = mkS 1 base n
  where
    base = fromIntegral (List.length alphabet)
    mkS :: Int -> Natural -> Natural -> String
    mkS !k !base' !n' =
      if base' > n'
        then
          List.map (alphabet!!) . List.reverse . List.take k $
            toRevDigits base n'
        else
          mkS (k + 1) (base' * base) (n' - base')

stringToNatural :: String -> Maybe Natural
stringToNatural s = do
  let
    base = fromIntegral (List.length alphabet)
    bases = List.iterate (*base) 1
  ns <-
    for @[] s $ \c ->
      fmap fromIntegral . listToMaybe $ List.findIndices (== c) alphabet
  let
    k = List.length ns
    offset = sum (List.take k bases) - 1
    n = (+offset) . sum . List.zipWith (*) bases . List.reverse $ ns
  return n

fingerprintToString :: Fingerprint -> String
fingerprintToString = naturalToString . fingerprintToNatural

stringToFingerprint :: String -> Maybe Fingerprint
stringToFingerprint = naturalToFingerprint <=< stringToNatural
