module Substitution (substitution) where
    
    import Crypto (eligible)
    import Data.Char (toUpper, isAlpha, isUpper, toLower)
    import Data.List (nub)
    import qualified Data.Map as Map
    
    type AlphaLetter = Char 
    type KeyLetter = Char
    type KeyValue = Map.Map KeyLetter AlphaLetter
    
    substitution :: String -> String -> String -> String
    substitution direction key = 
        map (\c -> if eligible c 
                            then substitutionAux adaptedKey c
                            else c)  
        where adaptedKey = adaptKey key direction
    
    substitutionAux :: KeyValue -> Char -> Char
    substitutionAux values c = getValue $ Map.lookup (toUpper c) values
                             where  getValue Nothing = ' '
                                    getValue (Just d) = if isUpper c then d else toLower d 

    -- Adapts key so it has unique elements and is concatenated with 
    -- the letters of the alphabet that are not contained in the key
    -- already.
    adaptKey :: String -> String -> KeyValue
    adaptKey key direction
        | direction == "enc" = Map.fromList $ zip alphabet filteredKey
        | direction == "dec" = Map.fromList $ zip filteredKey alphabet
        | otherwise = error "Invalid direction"
                             where filteredKey = unique ++ 
                                         filter (`notElem` unique) alphabet
                                   upperKey = map toUpper onlyAlpha
                                   unique = nub upperKey
                                   alphabet = ['A'..'Z']
                                   onlyAlpha = filter isAlpha key