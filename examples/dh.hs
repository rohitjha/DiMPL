-- Diffie - Hellman Key Exchange Protocol


import Modular
import Base
import Data.Char

prime = 5
root = 2

xa = 10
xb = 7

{-newtype Key a = Key (a, a) deriving (Show, Eq, Read)

getKey (Key a) = a-}

-- primitive root = alpha, prime = q
-- public key = (alpha ^ x) mod q
publicKey :: Integer -> Integer -> Integer -> Integer
publicKey alpha x q = modExp alpha x q


-- K = (Y ^ X) mod q
sharedKey :: Integer -> Integer -> Integer -> Integer
sharedKey pubKey priKey q = modExp pubKey priKey q

{-
main = do
    putStr "Enter primitive root: "
    alpha <- getLine
    putStr "Enter prime number: "
    q <- getLine
    putStr "Enter private key of A: "
    xA <- getLine
    putStr "Enter private key of B: "
    xB <- getLine
    --yA <- publicKey (read alpha) (Key $ read xA) (read q)
    --yB <- publicKey (read alpha) (read xB) (read q)
    --sKey <- sharedKey (Key $ read xA) (Key yB) (read q)
    putStrLn ("Public Key of A: " ++ show (Key $ (publicKey (read alpha) (Key $ read xA) (read q))))
    putStrLn ("Public Key of B: " ++ show (Key $ (publicKey (read alpha) (Key $ read xB) (read q))))
    --putStrLn ("Shared Key: " ++ show sKey)-}

str2int :: String -> Integer
str2int string = fromBase 10 (map char2int string)
    where
        char2int char = toInteger $ ord(char) - ord('0')


main = do
    putStrLn "Enter primitive root (alpha): "
    alpha <- getLine
    putStrLn "Enter prime number (q): "
    q <- getLine
    
    putStrLn "\nEnter A's private key (xA): "
    xA <- getLine
    putStrLn "Enter B's private key (xB): "
    xB <- getLine
    
    putStrLn ("\nCalculating Public Keys ...")
    putStrLn ("A's public key (yA): " ++ show (publicKey (str2int alpha) (str2int xA) (str2int q)))
    putStrLn ("B's public key (yB): " ++ show (publicKey (str2int alpha) (str2int xB) (str2int q)))
    
    putStrLn ("\nGenerating Shared Key ...")
    putStrLn ("Shared Key by A: " ++ show (sharedKey (publicKey (str2int alpha) (str2int xB) (str2int q)) (str2int xA) (str2int q)))
    putStrLn ("Shared Key by B: " ++ show (sharedKey (publicKey (str2int alpha) (str2int xA) (str2int q)) (str2int xB) (str2int q)))


{-
A's Public Key ----
publicKey 2 10 5
4

B's Public Key ----
publicKey 2 7 5
3

Calculating shared key (by B)
sharedKey 4 7 5
4

Calculating shared key (by A)
sharedKey 3 10 5
4
-}


{-
    Enter primitive root (alpha): 2
    Enter prime number (q): 5

    Enter A's private key (xA): 10
    Enter B's private key (xB): 7

    A's public key (yA): 4
    B's public key (yB): 3

    Generating Shared Key ...
    Shared Key by A: 4
    Shared Key by B: 4
-}
