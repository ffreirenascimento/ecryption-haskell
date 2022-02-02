module Substitution (substitution) where
    
    import Data.Char (toUpper)
    import Data.List (nub)

    type LetraAlfa = Char 
    type LetraChave = Char
    type MapaChave = [(LetraAlfa,LetraChave)]
    
    substitution :: String -> String -> String -> String
    substitution direction key content = error "TODO"

    adaptKey :: String -> MapaChave
    adaptKey key = zip alphabet filteredKey
                  where filteredKey = unique ++ filter (`notElem` unique) alphabet
                        upperKey = map toUpper key
                        unique = nub upperKey
                        alphabet = ['A'..'Z']