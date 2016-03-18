{-|
Module      : Combinatorics
Description : Combinatorics module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
  * Factorial
	* Finding number of combinations
	* Finding number of permutations
	* Generating permutations and combinations
	* Generating random permutation (shuffle)
-}

module Combinatorics
(
  factorial,
  c,
  p,
  permutation,
  shuffle,
  combination
)
where

import Data.List as L
import System.Random
import Control.Applicative



{-|
    The 'factorial' function returns the factorial of an integer.
    It takes one argument, of type 'Integer'.

    Below are a few examples:
  
    >>> factorial 5
    120
		
    >>> factorial 25
    15511210043330985984000000
	
    >>> factorial (-1)
    *** Exception: Usage - factorial n, where 'n' is non-negative.
 -}
factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n == 1 = 1
  | n > 1 = product [2..n]
  | n < 0 = error "Usage - factorial n, where 'n' is non-negative."


{-|
    The 'c' function returns the number of combinations.
    It takes two arguments, both of type 'Integer'.
    When called as c n r, where 'n' and 'r' are arguments, it returns the number of possible combinations of 'r' objects from a set of 'n' objects.

    Below are a few examples:
  
    >>> c 10 5
    252
		
    >>> 18 `c` 8
    43758
-}
c :: Integer -> Integer -> Integer
c n r
  | n < 1 = error "Usage - c n r, where 'n' is positive."
  | r < 1 = error "Usage - c n r, where 'r' is positive."
  | otherwise = product [(b+1) .. n] `div` product [1 .. (a-b)]
              where
                a = max n r
                b = min n r


{-|
    The 'p' function returns the number of permutations.
    It takes two arguments, both of type 'Integer'.
    When called as p n r, where 'n' and 'r' are arguments, it returns the number of possible permutations of 'r' objects from a set of 'n' objects.

    Below are a few examples:
  
    >>> p 10 5
    30240
		
    >>> 18 `p` 8
    1764322560
-}
p :: Integer -> Integer -> Integer
p n r
  | n < 1 = error "Usage - p n r, where 'n' is positive."
  | r < 1 = error "Usage - p n r, where 'r' is positive."
  | otherwise = product [(a-b+1) .. a]
                where
                  a = max n r
                  b = min n r


{-|
    The 'permutation' function returns all possible permutations of a list.
    It takes one argument, which can be a list or string.

    Below are a few examples:
  
    >>> permutation "abc"
    ["abc","bac","cba","bca","cab","acb"]
		
    >>> permutation [1,2]
    [[1,2],[2,1]]
-}
permutation :: [a] -> [[a]]
permutation = L.permutations


{-|
    The 'shuffle' function generates a random permutation.
    The function takes a list as argument and returns a shuffled list using the Fisher-Yates shuffle algorithm.

    Below are a few examples:

    >>> shuffle [1..10]
    [6,10,1,7,4,8,9,3,2,5]
		
    >>> shuffle [2,4..10]
    [8,4,10,2,6]
		
    >>> shuffle [2,3,5,7]
    [3,7,5,2]
		
    >>> shuffle ["abc", "xys"]
    ["abc","xys"]
		
    >>> shuffle ["abc", "xys", "fdvdfv"]
    ["abc","fdvdfv","xys"]
		
    >>> shuffle [[1,2], [3,4], [5,6]]
    [[3,4],[1,2],[5,6]]
-}
shuffle :: [a] -> IO [a]
shuffle l = shuffle' l []
  where
    shuffle' [] acc = return acc
    shuffle' l acc =
      do
        k <- randomRIO (0, length l - 1)
        let (lead, x:xs) = splitAt k l
        shuffle' (lead ++ xs) (x:acc)


{-|
    The 'combination' function generates all possible combinations.
    The function takes two arguments, the first of type Integer and the second as a list of lists.
    The function generates a combination of 'n' elements at a time, where 'n' is the first argument.

    Below are a few examples:

    >>> combination 2 ["a","b","c"]
    ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

    >>> combination 2 [[1],[2],[3]]
    [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
-}
combination n as = foldr1 prod $ replicate n as
      where
        prod as bs = (++) <$> as <*> bs
