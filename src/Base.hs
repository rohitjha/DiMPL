{-
-----------------------------
| Base/Radix Module for MPL |
-----------------------------

Functionality for:
	-> Converting decimal numbers to any base/radix
	-> Converting numbers from any base/radix to decimal base/radix

Author: Rohit Jha
Version: 0.1
Date: 31 Dec 2012
-}

module Base
(
	toBase,
	fromBase,
	toAlphaDigits,
	fromAlphaDigits
)
where

import Data.List
import Data.Char


-- Converting a number from decimal base/radix to specified base/radix in the form of a list of digits
{-
	Usage:
		toBase 8 37
		>>> [4,5]
		
		toBase 100 233243
		>>> [23,32,43]
		
		toBase 9 233243
		>>> [3,8,4,8,4,8]
		
		toBase 35 233243
		>>> [5,15,14,3]
-}
toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
	toBase' a 0 = a
	toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b


-- Converting a number from a specified radix/base to decimal in the form of a list of digits
{-
	Usage:
		fromBase 16 [15,15,15]
		>>> 4095
		
		fromBase 100 [21,12,1]
		>>> 211201
		
		fromBase 2 [1,0,1]
		>>> 5
-}
fromBase :: Int -> [Int] -> Int
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds


-- Converting a number from a list of Int to corresponding String representation
{-
	Usage:
		toAlphaDigits $ toBase 16 23432
		>>> "5b88"
		
		toAlphaDigits $ toBase 16 255
		>>> "ff"
		
		toAlphaDigits [38,12,1]
		>>> "}c1"
		
		toAlphaDigits [21,12,1]
		>>> "lc1"
-}
toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)


-- Converting a number from decimal to specified radix/base
{-
	Usage:
		fromAlphaDigits "j43hbrh"
		>>> [19,4,3,17,11,27,17]
		
		fromAlphaDigits "ffff"
		>>> [15,15,15,15]
-}
fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10

