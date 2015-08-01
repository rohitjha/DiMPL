-- Mersenne Prime Numbers

import Set
import Primes
import Base
import Data.Char


str2int :: String -> Integer
str2int string = fromBase 10 (map char2int string)
    where
        char2int char = toInteger $ ord(char) - ord('0')

--str2int string = fromIntegral $  (str2int' string)


mersPrimePowersTo n = Set { e | e <- primesTo n, isPrime (2^e - 1) }


mersPrimesToPower n = Set { 2^e - 1 | e <- primesTo n, isPrime (2^e - 1) }



main = do
    putStrLn "Choose an option:\n\t[1] : Mersenne Prime Powers upto a power 'n'\n\t[2] : Mersenne Prime Numbers upto a power 'n'\n\t[3] : Exit"
    option <- getLine
    
    case option of
        "1" -> do
            putStrLn "Enter power"
            power <- getLine
            putStrLn ("\nPowers - \n" ++ show (mersPrimePowersTo (str2int power)))
        "2" -> do
            putStrLn "Enter power"
            power <- getLine
            putStrLn ("\nNumbers - \n" ++ show (mersPrimesToPower (str2int power)))
        "3" -> do
            putStrLn "Exiting."

{-
Choose an option:
    [1] : Mersenne Prime Powers upto a power 'n'
    [2] : Mersenne Prime Numbers upto a power 'n'
    [3] : Exit
1
Enter power
100
Answer - 
[2,3,5,7,13,17,19,31,61,89]
-}

{-
Choose an option:
    [1] : Mersenne Prime Powers upto a power 'n'
    [2] : Mersenne Prime Numbers upto a power 'n'
    [3] : Exit
2
Enter power
100
Answer - 
[3,7,31,127,8191,131071,524287,2147483647,2305843009213693951,618970019642690137449562111]
-}

{-
Choose an option:
    [1] : Mersenne Prime Powers upto a power 'n'
    [2] : Mersenne Prime Numbers upto a power 'n'
    [3] : Exit
3
Exiting.
-}
