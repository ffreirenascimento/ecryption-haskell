# ecryption-haskell
This is a program that offers three different algorithms of encryption to encrypt and resolve text provided by the user.
The algorithms are:
1. Caesar: It is a type of substitution cipher in which each letter in the plaintext is replaced by a letter some fixed number of positions down the alphabet.
2. Vigenere: It is a method of encrypting alphabetic text by using a series of interwoven Caesar ciphers, based on the letters of a keyword. It employs a form of polyalphabetic substitution.
3. Substitution: A key is given and the same is manipulated to contain unique characters of the original key followed by all others characters of the alphabet that are not contained in the original key. The text given is ciphered by substituting each letter for their respective letter according to its position in the manipulated key. 
Example: 
  Original key      = PROGRAMACAO
  Manipulated key   = PROGAMCBDEFHIJKLNQSTUVWXYZ
  Original Alphabet = ABCDEFGHIJKLMNOPQRSTUVWXYZ
  
  Original text = As armas e os baroes assinalados
  Ciphered text = Ps pqips a ks rpqkas pssdjphpgks
*The program is not case sensitive, eather for the cipher key and for the text
 
How to run: 
1. ghc --make Main.hs
2. ./Main.hs method directionOfCipher key
After this type the text you want cyphered
*Files can be passed through pipes:
*./Main substitution enc PROGRAMING < text01P.txt


Valid inputs:
method = caesar | vigenere | substitution
directionOfCipher = enc | dec
key = any alpha string


Limitations:
*Only cipher characters in the alphabet, special characters and numbers are not considered 
