module Tests (test) where
    import Caesar
    import Vigenere
    import Substitution
    import Test.QuickCheck
    import Data.Char
    
    newtype ValidMessage = ValidMessage String deriving Show
    newtype ValidKey = ValidKey String deriving Show

    instance Arbitrary ValidMessage where
        arbitrary = ValidMessage . filter validChar <$> arbitrary
    
    instance Arbitrary ValidKey where
        arbitrary = ValidKey <$> 
            listOf (elements (['A'..'Z'] ++ ['a'..'z']))

    validChar :: Char -> Bool
    validChar c = isAlpha c || isSpace c || (isPunctuation c && (c /= ('\\'))) || isNumber c
    
    --1) Applying Caesar with a key 'n' followed by applying Caesar with a key m
    -- equals applying Caesar with key 'n' + 'm' to the same ValidMessage 
    prop_mult_caesar ::  ValidMessage -> Int -> Int -> Property 
    prop_mult_caesar (ValidMessage message) n m = n >= 0 && m >= 0 ==>
        caesar "enc" m (caesar "enc" n message) == caesar "enc" (n + m) message
    
    --2) Applying the cipher and resolving the message using the same key
    -- equals the original message:
    -- 1.Caesar
    prop_original_caesar :: ValidMessage -> Int -> Property
    prop_original_caesar (ValidMessage message) key = key >= 0 ==> 
        caesar "dec" key (caesar "enc" key message) == message
    
    -- 2.Vigenere
    prop_original_vigenere :: ValidMessage -> ValidKey -> Bool 
    prop_original_vigenere (ValidMessage message) (ValidKey key) =
        vigenere "dec" key (vigenere "enc" key message) == message
    
    -- 3.Substitution
    prop_original_substitution :: ValidMessage -> ValidKey -> Bool
    prop_original_substitution (ValidMessage message) (ValidKey key) = 
        substitution "dec" key (substitution "enc" key message) == message 
    
    --3) Applying the cipher to a message using a negative number as a key 
    -- is equal to resolving the original message with the same key
    prop_negative_number_caesar :: ValidMessage -> Int -> Bool 
    prop_negative_number_caesar (ValidMessage message) key =
        caesar "enc" ((-1) * key) message == caesar "dec" key message
    
    --4) The length of a message after the cipher is applied 
    -- is the same as the original message length
    prop_same_length_caesar_enc :: ValidMessage -> Int -> Bool
    prop_same_length_caesar_enc (ValidMessage message) key = 
        length (caesar "enc" key message) == length message
    
    prop_same_length_caesar_dec :: ValidMessage -> Int -> Bool
    prop_same_length_caesar_dec (ValidMessage message) key =
        length (caesar "dec" key message) == length message

    prop_same_length_vigenere_enc :: ValidMessage -> ValidKey -> Bool
    prop_same_length_vigenere_enc (ValidMessage message) (ValidKey key) =
        length (vigenere "enc" key message) == length message
    
    prop_same_length_vigenere_dec :: ValidMessage -> ValidKey -> Bool
    prop_same_length_vigenere_dec (ValidMessage message) (ValidKey key) =
        length (vigenere "dec" key message) == length message
    
    prop_same_length_substitution_enc :: ValidMessage -> ValidKey -> Bool
    prop_same_length_substitution_enc (ValidMessage message) (ValidKey key) =
        length (substitution "enc" key message) == length message

    prop_same_length_substitution_dec :: ValidMessage -> ValidKey -> Bool
    prop_same_length_substitution_dec (ValidMessage message) (ValidKey key) =
        length (substitution "dec" key message) == length message
    
    --5) The concatenated result of a cipher, with the same key, applied to two different messages
    -- is the same as the same cipher applied, with the same key, to the concatenated original messages

    prop_concat_caesar :: ValidMessage -> ValidMessage -> Int -> Bool
    prop_concat_caesar (ValidMessage message1) (ValidMessage message2) key =
        caesar "enc" key message1 ++ caesar "enc" key message2 == caesar "enc" key (message1 ++ message2)

----------------------------------------------------------------------------------

    test :: IO ()
    test = do 
                test1
                test2
                test3
                test4
                test5

    test1 :: IO ()
    test1 = quickCheck prop_mult_caesar

    test2 :: IO ()
    test2 = do 
                quickCheck prop_original_caesar 
                quickCheck prop_original_vigenere
                quickCheck prop_original_substitution
    
    test3 :: IO ()
    test3 = do 
                quickCheck prop_negative_number_caesar
    
    test4 :: IO ()
    test4 = do 
                quickCheck prop_same_length_caesar_enc 
                quickCheck prop_same_length_caesar_dec
                quickCheck prop_same_length_vigenere_enc
                quickCheck prop_same_length_vigenere_dec
                quickCheck prop_same_length_substitution_enc
                quickCheck prop_same_length_substitution_dec
    
    test5 :: IO ()
    test5 = do 
                quickCheck prop_concat_caesar 



