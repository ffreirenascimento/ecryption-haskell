module Vigenere (vigenere) where

    import Crypto
    import Data.Char (ord, toUpper)

    vigenere :: String -> String -> String -> String
    vigenere direction key content = vigenereAux direction cycledKey content
        where cycledKey = take (length (filter eligible content)) -- length of the content containing only eligible characters
                            $ cycle $ adaptKey key


    -- Recursively applies encode or decode functions to the given content
    -- until content equals an empty list 
    vigenereAux :: String -> [Int] -> String -> String
    vigenereAux _ _ [] = []
    vigenereAux _ [] cs = cs 
    vigenereAux direction key@(k:ks) (c:cs) 
        | direction == "enc" = if eligible c then encode k c : vigenereAux direction ks cs
                                else c : vigenereAux direction key cs
        | direction == "dec" = if eligible c then decode k c : vigenereAux direction ks cs
                                else c : vigenereAux direction key cs
        | otherwise = error "Invalid direction"

    -- Adapts the key to be values between 0 and 25 according
    -- to the positions of the character in the alphabet.
    -- Non-alphabetic characters will be ignored
    adaptKey :: String -> [Int]
    adaptKey key = map (abs. (65 -) . ord .toUpper) $ 
        filter eligible key 
