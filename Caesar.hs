module Caesar (caesar) where

import Data.Char (chr, ord)

caesar :: String -> Int-> String -> String
caesar _ _ [] = []
caesar direction key content = map (caesarAux direction key) content
                              
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
decode 0 c = c
decode key c = if res >= 97 
                then chr res
                else decode (97 `mod` key) (chr 122)
                where res = ord c - key

encode :: Int -> Char -> Char
encode 0 c = c
encode key c = if res >= 97 && res <= 122 
                then chr res
                else encode (res `mod` 122) (chr 97)
                where res = key + ord c

  


