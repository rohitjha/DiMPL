{-
-------------------------------
| Prime Number Module for MPL |
-------------------------------

Functionality for:
	-> Generating prime numbers upto a limit (Sieve of Eratosthenes)
	-> Generate first 'n' primes
	-> Generate the immediately next prime number greater than or equal to given number
	-> Primality testing using Trial division and Miller-Rabin test
	-> Prime factorization

Author: Rohit Jha
Version: 0.2
Date: 17 Jun 2015
-}

module MPL.NumberTheory.Primes
(
	primesTo,
	primesBetween,
	nPrimes,
	primesTo100,
	trialDivision,
	primesTo10000,
	isTrialDivisionPrime,
	isStrongPseudoPrime,
	isMillerRabinPrime,
	isPrime,
	nextPrime,
	primeFactors,
	uniquePrimeFactors
)
where

import Data.List (nub)

-- internal functions ----------------------
d `divides` n = n `mod` d == 0


n `splitWith` p = doSplitWith 0 n
	where doSplitWith s t
		| p `divides` t = doSplitWith (s+1) (t `div` p)
		| otherwise     = (s, t)


power (idG,multG) x n = doPower idG x n
	where
		doPower y _ 0 = y
		doPower y x n =
			let y' = if odd n then (y `multG` x) else y
			    x' = x `multG` x
			    n' = n `div` 2
			in doPower y' x' n'


minus (x:xs) (y:ys) = case (compare x y) of 
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


sieve (p:xs) 
       | p*p > (last xs)   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
----------------------------------------------


-- Generate list of primes upto specified limit by using Sieve of Eratosthenes
{-
	Usage:
		primesTo 20
		=> [2,3,5,7,11,13,17,19]
		primesTo 76
		=> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
-}
primesTo :: Integer -> [Integer]
primesTo m
	| (m < 0) = error "Usage - primesTo m, where 'm' is non-negative."
	| (m < 2) = []
	| (m == 2) = [2]
	| otherwise = 2 : sieve [3,5..m]


-- Generate all primes between two numbers (upper limit inclusive)
{-
	Usage:
		primesBetween 0 10
		>>> [2,3,5,7]

		primesBetween 10 25
		>>> [11,13,17,19,23]

		primesBetween 5 2
		>>> [2,3,5]
-}
primesBetween :: Integer -> Integer -> [Integer]
primesBetween m n
	| (m > n) = primesBetween n m
	| ((m < 0) || (n < 0)) = error "Usage - primesBetween m n, where m and n are non-negative."
	| (m <= 2) = primesTo n
	| otherwise = (primesTo n) `minus` (primesTo m)--(nextPrime m) : sieve [((nextPrime m)+1) .. n]


-- Generate list of first 'n' primes
{-
	Usage:
		nPrimes 10
		=> [2,3,5,7,11,13,17,19,23,29]
		nPrimes 42
		=> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181]
-}
nPrimes n = take n (sieve [2..])
	where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
	      notdiv p n = n `mod` p /= 0


-- List of prime numbers under 100
primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


-- Trial division
trialDivision ps n = doTrialDivision ps
	where doTrialDivision (p:ps) = let (q,r) = n `quotRem` p in if r == 0 then False else if q < p then True else doTrialDivision ps
	      doTrialDivision [] = True


-- List of prime numbers under 10000
primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]


-- Determine primality using trial division
{-
	Usage:
		isTrialDivisionPrime 83498594
		=> False
		isTrialDivisionPrime 83498594823549879237544
		=> False
-}
isTrialDivisionPrime 2 = True
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n


-- Check if number is a pseudo-prime (probable)
isStrongPseudoPrime :: Integer -> (Int,Integer) -> Integer -> Bool
isStrongPseudoPrime n (s,t) b =
	let b' = power (1, \x y -> x*y `mod` n) b t
	in if b' == 1 then True else doSquaring s b'
	where
		doSquaring 0 x = False
		doSquaring s x
			| x == n-1  = True
			| x == 1    = False
			| otherwise = doSquaring (s-1) (x*x `mod` n)


-- Check if number is prime using Miller-Rabin primality test
{-
	Usage:
		isMillerRabinPrime 34658273497773875
		=> False
		isMillerRabinPrime 34658273497773875327483287487877599
		=> True
-}
isMillerRabinPrime :: Integer -> Bool
isMillerRabinPrime n
	| n < 100   = n `elem` primesTo100
	| otherwise = all (isStrongPseudoPrime n (s,t)) primesTo100
		where (s,t) = (n-1) `splitWith` 2


-- Primality Checking which uses appropriate test according to the given number
{-
	Usage:
		isPrime (5^100)
		=> False
		isPrime 3973793488238
		=> False
		isPrime 199
		=> True
-}
isPrime :: Integer -> Bool
isPrime n
	| n < 2          = False
	| n < 500000000  = isTrialDivisionPrime n
	| n >= 500000000 = isMillerRabinPrime n


-- Generate the next prime greater than or equal to the given number
{-
	Usage:
		nextPrime 10240
		=> 10243
		nextPrime 3894345
		=> 3894347
-}
nextPrime :: Integer -> Integer
nextPrime n
	| (n > 0) = head [p | p <- [n..], isPrime p]
	| (n < 2) = 2


-- Prime factorization of a number
{-
	Usage:
		primeFactors 23984
		=> [2,2,2,2,1499]
		primeFactors 273219943
		=> [19,89,161573]
-}
primeFactors :: Integer -> [Integer]
primeFactors n
	| (n < 1) = error "Usage - primeFactors n, where n is a positive integer."
	| (isPrime n) = [n]
	| otherwise = concat (map (\(p,a) -> replicate a p) (primePowerFactors n))


-- Unique prime factors of a number
{-
 	Usage:
		uniquePrimeFactors 10000000
		>>> [2,5]
		uniquePrimeFactors 123456789
		>>> [3,3607,3803]
-}
uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors n
	| (n < 1) = error "Usage - uniquePrimeFactors n, where n is a positive number."
	| otherwise = nub $ primeFactors n 
