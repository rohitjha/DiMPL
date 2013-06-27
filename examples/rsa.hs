-- Implementation of the RSA algorithm in Haskell


import MPL.NumberTheory.Primes
import MPL.NumberTheory.Modular
import MPL.NumberTheory.Base
import Data.Char


-- Product of two primes
n :: Int -> Int -> Int
n p q =
	if ((isPrime (fromIntegral p)) && (isPrime (fromIntegral q))) == False
	then error "The two numbers must be primes"
	else fromIntegral (p * q)


-- Euler Totient function (phi)
eulerTotient :: Int -> Int -> Int
eulerTotient p q = (p-1) * (q-1)


-- Encryption Key 'e'
encryptionKey :: Int -> Int
encryptionKey phi = [i | i <- [3 .. (phi-1)], (gcd i phi) == 1] !! 0


-- Public Key (e,n)
publicKey :: Int -> Int -> (Int, Int)
publicKey p q = ((encryptionKey phi), (n p q))
	where
		phi = eulerTotient p q


-- Decryption Key 'd'
-- finding the first congruent pair upto 'limit'
decryptionKey :: Int -> Int -> Int
decryptionKey e phi = (findCongruentPair e 1 phi phi) !! 0
	where
		limit = 2^64


-- Private Key (d,n)
privateKey :: Int -> Int -> (Int, Int)
privateKey p q = ((decryptionKey e phi), (n p q))
	where
		e = fst (publicKey p q)
		phi = eulerTotient p q


-- Encrypt 'message' using the public-key 'pubKey'
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt message pubKey = modExp message e n'
	where
		e = fst pubKey
		n' = snd pubKey


rsaEncryptL :: [Int] -> (Int,Int) -> [Int]
rsaEncryptL message pubKey = [rsaEncrypt m pubKey | m <- message]
{-	where
		e = fst pubKey
		n' = snd pubKey
-}

-- Decrypt 'cipher' using the private-key 'privKey'
rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt cipher privKey = (modExp cipher d n') `mod` n'
	where
		d = fst privKey
		n' = snd privKey


rsaDecryptL :: [Int] -> (Int,Int) -> [Int]
rsaDecryptL cipher privKey = [rsaDecrypt c privKey | c <- cipher]
{-	where
		d = fst privKey
		n' = snd privKey
-}

str2int :: String -> Int
str2int string = fromBase 10 (map char2int string)
	where
		char2int char = ord(char) - ord('0')


main = do
	putStrLn "Choose an operation -\n\t[1]: Encryption\n\t[2]: Decryption\n\t[3]: Exit\n>>"
	operation <- getLine
	case operation of
		"1" -> do
			putStrLn "\nEnter first part of Public Key: "
			pubK1 <- getLine
			putStrLn "\nEnter second part of Public Key: "
			pubK2 <- getLine
			putStrLn "\nEnter Message: "
			mess <- getLine
			putStrLn ("\nEncrypted message:\n" ++ show (rsaEncryptL (read mess) ((str2int pubK1),(str2int pubK2))))
		"2" -> do
			putStrLn "\nEnter first part of Private Key: "
			priK1 <- getLine
			putStrLn "\nEnter second part of Private Key: "
			priK2 <- getLine
			putStrLn "\nEnter Cipher: "
			ciph <- getLine
			putStrLn ("\nDecrypted message:\n" ++ show (rsaDecryptL (read ciph) ((str2int priK1),(str2int priK2))))
		"3" -> do
			putStrLn "Exiting."


--main = rsa


-- RSA Encryption and Decryption Examples
{-
publicKey 11 13
(7,143)
*Main> privateKey 11 13
(103,143)
-}
{-
	rsaEncrypt 14 (publicKey 3 11)
	>>> 5
	rsaDecrypt 5 (privateKey 3 11)
	>>> 14
-}

{-
	rsaEncrypt 100 (publicKey 13 17)
	>>> 172
	rsaDecrypt 172 (privateKey 13 17)
	>>> 100
-}

{-
	rsaDecrypt 219 (privateKey 13 17)
	>>> 189
	rsaDecrypt 189 (privateKey 13 17)
	>>> 219
-}

{-
	rsaEncrypt 400 (publicKey 23 29)
	>>> 16
	rsaDecrypt 16 (privateKey 23 29)
	>>> 400
-}

{-
	rsaEncrypt 500 (publicKey 29 31)
	>>> 748
	rsaDecrypt 748 (privateKey 29 31)
	>>> 500
-}

{-
	rsaEncrypt 300 (publicKey 31 37)
	>>> 104
	rsaDecrypt 104 (privateKey 31 37)
	>>> 300
-}

{-
	rsaEncrypt 400 (publicKey 37 41)
	>>> 1335
	rsaDecrypt 1335 (privateKey 37 41)
	>>> 400
-}

{-
	rsaEncrypt 500 (publicKey 41 43)
	>>> 1673
	rsaDecrypt 1673 (privateKey 41 43)
	>>> 500
-}

{-
	rsaEncrypt 1300 (publicKey 41 49)
	>>> *** Exception: The two numbers must be primes
-}

{-
	rsaEncrypt 1300 (publicKey 41 47)
	>>>322
	rsaDecrypt 322 (privateKey 41 47)
	>>> 1300
-}

{-
	rsaEncrypt 600 (publicKey 89 97)
	>>> 493
	rsaDecrypt 493 (privateKey 89 97)
	>>> 600
-}

{-
	rsaEncrypt 700 (publicKey 197 199)
	>>> 4300
	rsaDecrypt 4300 (privateKey 197 199)
	>>> 700
-}

{-
	rsaEncrypt 1000 (publicKey 283 293)
	>>> 59611
	rsaDecrypt 59611 (privateKey 283 293)
	>>> 1000
-}

{-
	rsaEncrypt 2000 (publicKey 1021 1019)
	>>> 821921
	rsaDecrypt 821921 (privateKey 1021 1019)
	>>> 2000
-}

{-
	1. Encryption
	-------------
	Choose an operation -
		[1]: Encryption
		[2]: Decryption
		[3]: Exit
	>>1

	Enter first part of Public Key: 7

	Enter second part of Public Key: 143

	Enter Message: 14

	Encrypted message: 53


	2. Decryption
	-------------
	Choose an operation -
		[1]: Encryption
		[2]: Decryption
		[3]: Exit
	>>2

	Enter first part of Private Key: 103

	Enter second part of Private Key: 143

	Enter Cipher: 53

	Decrypted message: 14
-}
