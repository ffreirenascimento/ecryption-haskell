module Main where
import System.Environment
import Caesar
import Vigenere
import Substitution
import Control.Monad (unless, when)

main = do
    -- Obtain and validate arguments
    args@(method:direction:_) <- getArgs
    -- arguments should be in the form:
    -- method direction key
    if method `notElem` ["caesar","vigenere","substitution"]
        then 
            putStrLn $ "Invalid method" ++ "\n" ++
                       "Please use one of the following:" ++ "\n" ++
                       " > caesar" ++ "\n" ++
                       " > vigenere" ++ "\n" ++
                       " > substitution"
        else
            if direction `notElem` ["enc","dec"]
                then 
                    putStrLn $ "Invalid direction of encryption" ++ "\n" ++
                               "Please use one of the following:" ++ "\n" ++
                               " > enc" ++ "\n" ++
                               " > dec"
                else 
                    applyAlgorithms args

applyAlgorithms args = do
                    let args1 = args!!1
                        args2 = args!!2
                    contents <- getLine  
                    -- An empty line will close the program
                    when (not $ null contents) $ do
                        case head args of "caesar" -> putStrLn $ caesar args1 (read args2 :: Int) contents
                                          "vigenere" -> putStrLn $ vigenere args1 args2 contents
                                          "substitution" -> putStrLn $ substitution args1 args2 contents
                                          _ -> putStrLn "Invalid method"
                        applyAlgorithms args                  