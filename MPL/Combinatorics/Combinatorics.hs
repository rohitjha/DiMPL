{-

Combinatorics Module for MPL
----------------------------

Functionality for:
	-> Factorial
	-> Finding number of combinations
	-> Finding number of permutations
	-> Generating permutations and combinations
	-> Generating random permutation

Author: Rohit Jha
Version: 0.1.1
Date: 30 Mar 2014
-}

module MPL.Combinatorics.Combinatorics
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


-- Factorial function
{-
	Usage:
		factorial 5
		>>> 120
		
		factorial 25
		>>> 15511210043330985984000000
		
		factorial (-1)
		>>> *** Exception: Usage - factorial n, where 'n' is non-negative.
-}
factorial :: Integer -> Integer
factorial n
	| (n == 0) = 1
	| (n == 1) = 1
	| (n > 1) = product [2..n]
	| (n < 0) = error "Usage - factorial n, where 'n' is non-negative."


-- nCr
{-
	Usage:
		c 10 5
		>>> 252
		
		18 `c` 8
		>>> 43758
-}
c :: Integer -> Integer -> Integer
c n r = (factorial a) `div` ( (factorial b) * (factorial (a-b)) )
	where
	a = max n r
	b = min n r


-- nPr
{-
	Usage:
		p 10 5
		>>> 30240
		
		18 `p` 8
		>>> 1764322560
-}
p :: Integer -> Integer -> Integer
p n r = (factorial a) `div` (factorial (a-b))
	where
	a = max n r
	b = min n r


-- Permutation generation function
{-
	Usage:
		permutation "abc"
		>>> ["abc","bac","cba","bca","cab","acb"]
		
		permutation [1,2]
		>>> [[1,2],[2,1]]
-}
permutation :: [a] -> [[a]]
permutation x = L.permutations x


-- Random permutation generation - Fisher-Yates shuffle algorithm
{-
	Usage:
		shuffle [1..10]
		>>> [6,10,1,7,4,8,9,3,2,5]
		
		shuffle [2,4..10]
		>>> [8,4,10,2,6]
		
		shuffle [2,3,5,7]
		>>> [3,7,5,2]
		
		shuffle ["abc", "xys"]
		>>> ["abc","xys"]
		
		shuffle ["abc", "xys", "fdvdfv"]
		>>> ["abc","fdvdfv","xys"]
		
		shuffle [[1,2], [3,4], [5,6]]
		>>> [[3,4],[1,2],[5,6]]
-}
shuffle :: [a] -> IO [a]
shuffle l = shuffle' l []
	where
		shuffle' [] acc = return acc
		shuffle' l acc =
			do
				k <- randomRIO (0, (length l) - 1)
				let (lead, x:xs) = splitAt k l
				shuffle' (lead ++ xs) (x:acc)


-- Generating combination (n at a time, with repetition)
{-
	Usage:
		combination 2 ["a","b","c"]
		>>> ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

		combination 3 ["a","b","c"]
		>>> ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
-}
prod as bs = (++) <$> as <*> bs

combination n as = foldr1 prod $ replicate n as
