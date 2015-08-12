{-|
Module      : Fibonacci
Description	: Fibonacci sequence module for the MPL DSL
Copyright	  : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
  * Calculating the nth term in the Fibonacci Series
  * Generating Fibonacci Series with n terms
-}

module Fibonacci
(
  fib,
  fibSeq,
  fibIndex,
  isFibNum
)
where

import Data.List (findIndex)
import Data.Maybe (fromJust)


-- The first term in the Fibonacci Sequence
first = 0

-- The second term in the Fibonacci Sequence
second = 1

-- Function to generate the entire Fibonacci sequence
fibs = first : second : zipWith (+) fibs (tail fibs)


{-|
  	The 'fib' function returns the nth term of the Fibonacci sequence.
  	The function takes one argument of type Integer, whose value should be positive.

  	Below are a few examples:

  	>>> fib 10
  	34

  	>>> fib 100
  	218922995834555169026
-}
fib :: Integer -> Integer
fib n
    | n > 0 = fibs !! fromInteger (n - 1)
    | otherwise = error "Usage - fib n, where n is a positive integer."


{-|
  	The 'fibSeq' function returns the first n terms of the Fibonacci sequence.
  	The function takes one argument of type Integer.

  	Below are a few examples:

  	>>> fibSeq 10
  	[0,1,1,2,3,5,8,13,21,34]
  	
  	>>> fibSeq 20
  	[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
-}
fibSeq :: Integer -> [Integer]
fibSeq n
    | n > 0 = take (fromInteger n) fibs
    | otherwise = error "Usage - fibSeq n, where n is a positive integer."


{-|
  	The 'isFibNum' function checks whether an integer is a Fibonacci number.
  	The function takes one argument of type Integer.

  	Below are a few examples:

  	>>> isFibNum 144
  	True

  	>>> isFibNum (144 + 200)
  	False

  	>>> isFibNum (fib 1000)
  	True
-}
isFibNum :: Integer -> Bool
isFibNum f
    | f > 0 = head (dropWhile (< f) fibs) == f
    | otherwise = error "Usage - isFibNum f, where f is a positive integer."


{-|
  	The 'fibIndex' function returns the index of the passed Fibonacci number in the Fibonacci sequence.
  	The function takes one argument of type Integer.

  	Below are a few examples:

  	>>> fibIndex 144
  	13

  	>>> fibIndex 6765
  	21

  	>>> fibIndex (fib 10000)
  	10000
-}
fibIndex :: Integer -> Integer
fibIndex f
    | (f > 0) && isFibNum f = toInteger $ 1 + (fromJust $ findIndex (== f) fibs)
    | otherwise = error "Usage - fibIndex f, where f is a Fibonacci Series term."
