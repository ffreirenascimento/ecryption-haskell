module Caesar (caesar) where

import Crypto

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