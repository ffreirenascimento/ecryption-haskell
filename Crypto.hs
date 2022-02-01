module Crypto (eligible, encode, decode) where
-- This module provides functions to encode and
-- decode a character according to the given key
-- and a predicate to verify if a given character
-- is a capital or small letter 
-- Encoding will increase the value in ASCII of 
-- the character
-- Decoding will decrease the value

    import Data.Char (chr, ord, isUpper)

    eligible :: Char -> Bool
    eligible d = ((ord d) >= 65 && (ord d) <= 90) || ((ord d) >= 97 && (ord d) <= 122) 

    decode :: Int -> Char -> Char
    decode key c
        | isUpper c = if res < 65 then chr (91 - (65 `mod` res)) else chr res
        | otherwise = if res < 97 then chr (123 - (97 `mod` res)) else chr res
        where res = ord c - key

    encode :: Int -> Char -> Char
    encode key c
        | isUpper c = if res > 90 then chr (64 + (res `mod` 90)) else chr res
        | otherwise = if res > 122 then chr (96 + (res `mod` 122)) else chr res
        where res = key + ord c
    

