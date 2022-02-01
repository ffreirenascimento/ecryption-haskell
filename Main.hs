module Main where
import System.Environment
import Caesar
import Vigenere

main = do
    -- Obtain and validate arguments
    args <- getArgs
    -- arguments should be in the form:
    -- method direction key
    if head args `notElem` ["caesar","vinegere","substitution"]
        then 
            putStrLn $ "Invalid method" ++ "\n" ++
                       "Please use one of the following:" ++ "\n" ++
                       " > caesar" ++ "\n" ++
                       " > vigenere" ++ "\n" ++
                       " > substitution"
        else
            if args!!1 `notElem` ["enc","dec"]
                then 
                    putStrLn $ "Invalid direction of encryption" ++ "\n" ++
                               "Please use one of the following:" ++ "\n" ++
                               " > enc" ++ "\n" ++
                               " > dec"
                else 
                    do
                    let args1 = args!!1
                        args2 = args!!2
                    contents <- getContents 
                    case head args of "caesar" -> putStrLn $ caesar args1 (read args2 :: Int) contents
                                      "vigenere" -> putStrLn $ vigenere args1 args2 contents
                                      "substitution" -> putStrLn $ substitution args1 args2 contents
                                      _ -> return () -- Add error message
                                            