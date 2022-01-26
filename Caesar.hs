module Caesar (caesar) where

import Data.Char (chr, ord, isUpper)

caesar :: String -> Int-> String -> String
caesar _ _ [] = []
caesar direction key content = map (caesarAux direction (key `mod` 25)) content
                              
-- According to the direction and key, this method will
-- encrypt or solve with the given content using the 
-- given key
caesarAux :: String -> Int -> Char -> Char
caesarAux direction key c
    | direction == "enc" = if eligible c then encode key c else c
    | direction == "dec" = if eligible c then decode key c else c
    | otherwise = error "invalid input"

eligible :: Char -> Bool
eligible d = ((ord d) >= 65 && (ord d) <= 90) || ((ord d) >= 97 && (ord d) <= 122) 

decode :: Int -> Char -> Char
decode key c
    | isUpper c = if res < 65 then chr (90 - (65 `mod` res)) else chr res
    | otherwise = if res < 97 then chr (122 - (97 `mod` res)) else chr res
    where res = ord c - key

encode :: Int -> Char -> Char
encode key c
    | isUpper c = if res > 90 then chr (65 + (res `mod` 90)) else chr res
    | otherwise = if res > 122 then chr (97 + (res `mod` 122)) else chr res
    where res = key + ord c
