{-|
Module      : Base
Description : Base/radix module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
	* Converting decimal numbers to binary, octal, hexadecimal or any other base/radix
	* Converting numbers from binary, octal, hexadecimal or any other base/radix to decimal base/radix
-}

module Base
(
  toBase,
  toBin,
  toOct,
  toDec,
  toHex,
  fromBase,
  fromBin,
  fromOct,
  fromDec,
  fromHex,
  toAlpha,
  fromAlpha
)
where

import Data.List
import Data.Char



{-|
  	The 'toBase' function converts a number from decimal base to a specified base in the form of digits.
  	The function takes two arguments, both of type Integer.

  	Below are a few examples:

  	>>> toBase 8 37
  	[4,5]
		
  	>>> toBase 100 233243
  	[23,32,43]
		
  	>>> toBase 9 233243
  	[3,8,4,8,4,8]
		
  	>>> toBase 35 233243
  	[5,15,14,3]
-}
toBase :: Integer -> Integer -> [Integer]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b


{-|
  	The 'toBin' function converts a decimal number to its binary equivalent.
  	The function takes one argument of type Integer, which is the decimal number.

  	Below are a few examples:

  	>>> toBin 11
  	[1,0,1,1]
  	
  	>>> toBin 32
  	[1,0,0,0,0,0]

  	>>> toBin (3^(-1))
  	*** Exception: Negative exponent
-}
toBin :: Integer -> [Integer]
toBin = toBase 2


{-|
    The 'toOct' function converts a decimal number to its octal equivalent.
  	The function takes one argument of type Integer, which is the decimal number.

  	Below are a few examples:

  	>>> toOct 11
  	[1,3]

  	>>> toOct 100
  	[1,4,4]
-}
toOct :: Integer -> [Integer]
toOct = toBase 8


{-|
  	The 'toDec' function converts a decimal number to its decimal equivalent.
  	The function returns a list of digits of the decimal number.
  	The function takes one argument of type Integer, which is the decimal number.

  	Below are a few examples:

  	>>> toDec 15
  	[1,5]

  	>>> toDec 123
  	[1,2,3]
-}
toDec :: Integer -> [Integer]
toDec = toBase 10


{-|
	The 'toHex' function converts a decimal number to its hexadecimal equivalent.
  	The function takes one argument of type Integer, which is the decimal number.

  	Below are a few examples:

  	>>> toHex 15
  	[15]

  	>>> toHex 1200
  	[4,11,0]
-}
toHex :: Integer -> [Integer]
toHex = toBase 16


{-|
  	The 'fromBase' function converts a number from a specified base to decimal in the form of a list of digits.
  	The function takes two arguments, the first of type Integer and the second of type [Integer].

  	Below are a few examples:

  	>>> fromBase 16 [15,15,15]
  	4095
		
  	>>> fromBase 100 [21,12,1]
  	211201
		
  	>>> fromBase 2 [1,0,1]
  	5
-}
fromBase :: Integer -> [Integer] -> Integer
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds


{-|
  	The 'fromBin' function converts a number from binary (in the form of a list of digits) to decimal.
  	The function takes two arguments, the first of type Integer and the second of type [Integer].

  	Below are a few examples:

  	>>> fromBin [1,0,1,0]
    10

  	>>> fromBin (toBin 12345)
  	12345
-}
fromBin :: [Integer] -> Integer
fromBin = fromBase 2


{-|
  	The 'fromOct' function converts a number from octal (in the form of a list of digits) to decimal.
  	The function takes two arguments, the first of type Integer and the second of type [Integer].

  	Below are a few examples:

  	>>> fromOct [6,3,7]
    415

  	>>> fromOct (toOct 1234)
  	1234
-}
fromOct :: [Integer] -> Integer
fromOct = fromBase 8


{-|
  	The 'fromDec' function converts a number from decimal (in the form of a list of digits) to decimal.
  	The function takes two arguments, the first of type Integer and the second of type [Integer].

  	Below are a few examples:

  	>>> fromDec [1,5,8,2,2]
  	15822

  	>>> fromDec (toDec 635465)
  	635465
-}
fromDec :: [Integer] -> Integer
fromDec = fromBase 10


{-|
  	The 'fromHex' function converts a number from hexadecimal (in the form of a list of digits) to decimal.
  	The function takes two arguments, the first of type Integer and the second of type [Integer].

  	Below are a few examples:

  	>>> fromHex [14,15,8,2,0]
  	981024

  	>>> fromHex (toHex 0234)
  	234
-}
fromHex :: [Integer] -> Integer
fromHex = fromBase 16


{-|
  	The 'toAlpha' function converts a number from a list of Integer to corresponding String representation.
  	The function takes one argument of type [Integer], which contains the list of digits.
	
  	Below are a few examples:
  
  	>>> toAlpha $ toBase 16 23432
  	"5b88"
		
  	>>> toAlpha $ toBase 16 255
  	"ff"
		
  	>>> toAlpha [38,12,1]
  	"}c1"
		
  	>>> toAlpha [21,12,1]
  	"lc1"
-}
toAlpha :: [Integer] -> String
toAlpha = map convert where
  convert n | n < 10    = chr (fromInteger n + ord '0')
            | otherwise = chr (fromInteger n + ord 'a' - 10)


{-|
  	The 'from AlphaDigits' function converts a String to list of Integer digits.
  	The function takes only one argument of type String.

  	Below are a few examples:

  	>>> fromAlpha "j43hbrh"
  	[19,4,3,17,11,27,17]
  		
  	>>> fromAlpha "ffff"
  	[15,15,15,15]
-}
fromAlpha :: String -> [Integer]
fromAlpha = map convert where
 convert c | isDigit c = toInteger (ord c - ord '0')
           | isUpper c = toInteger (ord c - ord 'A' + 10)
           | isLower c = toInteger (ord c - ord 'a' + 10)

