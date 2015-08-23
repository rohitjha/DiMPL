{-|
Module      : Primes
Description : Prime Numbers module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
    * Generating prime numbers upto a limit (Sieve of Eratosthenes)
    * Generate first 'n' primes
    * Generate the immediately next prime number greater than or equal to given number
    * Primality testing using Trial division and Miller-Rabin test
    * Prime factorization
-}

module Primes
(
    primesTo,
    primesBetween,
    nPrimes,
    isPrime,
    nextPrime,
    primeFactors,
    uniquePrimeFactors,
    areCoprime
)
where

import Data.List (nub)


-- internal functions ----------------------
divides :: Integral a => a -> a -> Bool
d `divides` n = n `mod` d == 0


splitWith :: (Integral t, Num t1) => t -> t -> (t1, t)
n `splitWith` p = doSplitWith 0 n
    where
        doSplitWith s t
            | p `divides` t = doSplitWith (s+1) (t `div` p)
            | otherwise     = (s, t)


power :: Integral a => (t, t -> t -> t) -> t -> a -> t
power (idG, multG) = doPower idG
    where
        doPower y _ 0 = y
        doPower y x n =
            let y' = if odd n
                     then y `multG` x
                     else y
                x' = x `multG` x
                n' = n `div` 2
            in doPower y' x' n'


minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case compare x y of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs


primePowerFactors :: Integer -> [(Integer,Int)]
primePowerFactors n | n > 0 = takeOutFactors n primesTo10000
    where
        takeOutFactors n (p:ps)
            | p*p > n   = finish n
            | otherwise =
                let (s,n') = n `splitWith` p
                in if s > 0 then (p,s) : takeOutFactors n' ps else takeOutFactors n ps
        takeOutFactors n [] = finish n
        finish 1 = []
        finish n =
            if n < 100000000 || isMillerRabinPrime n
            then [(n,1)]
            else error ("primePowerFactors: unable to factor " ++ show n)


sieve :: (Enum a, Num a, Ord a) => [a] -> [a]
sieve (p:xs) 
       | p*p > last xs   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
----------------------------------------------


{-|
    The 'primesTo' function generates a list of prime numbers upto a specified limit by using Sieve of Eratosthenes

    For example:

    >>> primesTo 20
    [2,3,5,7,11,13,17,19]
    
    >>> primesTo 76
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
-}
primesTo :: Integer -> [Integer]
primesTo m
    | m < 0 = error "Usage - primesTo m, where 'm' is non-negative."
    | m < 2 = []
    | m == 2 = [2]
    | m < 5 = [2,3]
    | m == 10 = [2,3,5,7]
    | m == 100 = primesTo100
    | m == 10000 = primesTo10000
    | otherwise = 2 : sieve [3,5..m]


{-|
    The 'primesBetween' function returns all prime numbers between the mention lower and upper limits (upper limit inclusive)
    
    For example:

    >>> primesBetween 0 10
    [2,3,5,7]

    >>> primesBetween 10 25
    [11,13,17,19,23]

    >>> primesBetween 5 2
    [2,3,5]
-}
primesBetween :: Integer -> Integer -> [Integer]
primesBetween m n
    | m > n = primesBetween n m
    | m < 0 || n < 0 = error "Usage - primesBetween m n, where m and n are non-negative."
    | m <= 2 = primesTo n
    | otherwise = primesTo n `minus` primesTo m --(nextPrime m) : sieve [((nextPrime m)+1) .. n]


{-|
    The 'nPrimes' function returns a list of first 'n' primes, where 'n' is the argument passed.

    For example:

    >>> nPrimes 10
    [2,3,5,7,11,13,17,19,23,29]

    >>> nPrimes 42
    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181]
-}
nPrimes :: Integral a => Int -> [a]
nPrimes n = take n (sieve [2..])
    where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
          notdiv p n = n `mod` p /= 0


{-
    The 'primesTo100' function returns a list of all prime numbers less than 100.
-}
primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


-- internal function for trial division
trialDivision :: Integral a => [a] -> a -> Bool
trialDivision ps n = doTrialDivision ps
    where doTrialDivision (p:ps) = let (q,r) = n `quotRem` p in ((r /= 0) && ((q < p) || doTrialDivision ps))
          doTrialDivision [] = True


{-
    The 'primesTo100' function returns a list of all prime numbers less than 10000.
-}
primesTo10000 :: [Integer]
primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]


{-
    The 'isTrialDivisionPrime' function is checks the primality of a number using trial division.
    
    For example:

    >>> isTrialDivisionPrime 83498594
    False

    >>> isTrialDivisionPrime 83498617
    True

    >>> isTrialDivisionPrime 83498594823549879237544
    False
-}
isTrialDivisionPrime :: Integer -> Bool
isTrialDivisionPrime 2 = True
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n


{-
    The 'isStrongPseudoPrime' function is an internal function to check if number is a pseudo-prime (probable)
-}
isStrongPseudoPrime :: Integer -> (Int,Integer) -> Integer -> Bool
isStrongPseudoPrime n (s,t) b =
    let b' = power (1, \x y -> x*y `mod` n) b t
    in ((b' == 1) || doSquaring s b')
    where
        doSquaring 0 x = False
        doSquaring s x
            | x == n-1  = True
            | x == 1    = False
            | otherwise = doSquaring (s-1) (x*x `mod` n)


{-
    The 'isMillerRabinPrime' function checks if number is prime using Miller-Rabin primality test
    
    For example:

    >>> isMillerRabinPrime 34658273497773875
    False
    
    >>> isMillerRabinPrime 34658273497773875327483287487877599
    True
-}
isMillerRabinPrime :: Integer -> Bool
isMillerRabinPrime n
    | n < 100   = n `elem` primesTo100
    | otherwise = all (isStrongPseudoPrime n (s,t)) primesTo100
        where (s,t) = (n-1) `splitWith` 2


{-|
    The 'isPrime' function checks if a number is prime.

    The function uses the 'isTrialDivisionPrime' or 'isMillerRabinPrime' functions to test the primality.

    For example:

    >>> isPrime (5^100)
    False
    
    >>> isPrime 3973793488238
    False
    
    isPrime 199
    >>> True
-}
isPrime :: Integer -> Bool
isPrime n
    | n < 2          = False
    | n < 500000000  = isTrialDivisionPrime n
    | n >= 500000000 = isMillerRabinPrime n


{-|
    The 'nextPrime' function returns the next prime greater than or equal to the passed argument.
    
    For example:

    >>> nextPrime 10240
    10243
    
    >>> nextPrime 3894345
    3894347
-}
nextPrime :: Integer -> Integer
nextPrime n
    | n > 0 = head [p | p <- [n..], isPrime p]
    | n < 2 = 2


{-|
    The 'primeFactors' function returns all the prime factors of a number.

    The function includes repetitions of factors, as shown in the examples below. To get only the unique factors, use the 'uniquePrimeFactors' function.

    For example:
    
    >>> primeFactors 23984
    [2,2,2,2,1499]
    
    >>> primeFactors 273219943
    [19,89,161573]
-}
primeFactors :: Integer -> [Integer]
primeFactors n
    | n < 1 = error "Usage - primeFactors n, where n is a positive integer."
    | isPrime n = [n]
    | otherwise = concatMap (\ (p, a) -> replicate a p) (primePowerFactors n)


{-|
    The 'uniquePrimeFactors' function returns a single instance of all the prime factors of a number.

    For example:
    
    >>> uniquePrimeFactors 10000000
    [2,5]
    >>> uniquePrimeFactors 123456789
    [3,3607,3803]
-}
uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors n
    | n < 1 = error "Usage - uniquePrimeFactors n, where n is a positive number."
    | otherwise = nub $ primeFactors n 


{-|
    The 'areCoprime' function checks if two numbers are coprime / relatively prime / mutually prime.

    For example:

    >>> areCoprime 2 3
    True

    >>> areCoprime 100 200
    False
-}
areCoprime :: Integer -> Integer -> Bool
areCoprime a b = gcd a b == 1
