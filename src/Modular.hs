{-
-------------------------------------
| Modular Arithmetic Module for MPL |
-------------------------------------

Functionality for:
	-> Modular addition
	-> Modular subtraction
	-> Modular multiplication
	-> Modular exponentiation
	-> Checking congruence
	-> Solving linear congruences

Author: Rohit Jha
Version: 0.1
Date: 22 Jan 2013
-}


module Modular
(
	modAdd,
	modSub,
	modMult,
	modExp,
	isCongruent,
	findCongruentPair,
	findCongruentPair1
)
where


-- addition using modular arithmetic
-- (a + b) mod m = (a mod m) + (b mod m)
{-
	Usage:
		modAdd 17 44 11
		>>> 6
		
		modAdd 126832 1832 11
		>>> 8
-}
modAdd a b m = (a `mod` m) + (b `mod` m)


-- subtraction using modular arithmetic
-- (a - b) mod m = (a mod m) - (b mod m)
{-
	Usage:
		modSub 117 14 11
		>>> 4
		
		modSub 114787 23934 3874
		>>> 1751
-}
modSub a b m = (a `mod` m) - (b `mod` m)


-- multiplication using modular arithmetic
-- (a * b) mod m = ( (a mod m) * (b mod m) ) mod m
{-
	Usage:
		modMult 117 14 11
		>>> 10
		
		modMult 114787 23934 3874
		>>> 2974
-}
modMult a b m = ( (a `mod` m) * (b `mod` m) ) `mod` m


-- Modular Exponentiation operation - a^b mod m
{-
	Usage:
		modExp 12 5 6
		>>> 0
		
		modExp 112 34 546
		>>> 532
		
		modExp 515 5151 1563
		>>> 1004
-}
modExp a b m = modexp' 1 a b
	where
		modexp' p _ 0 = p
		modexp' p x b =
			if even b
				then modexp' p (mod (x*x) m) (div b 2)
			else modexp' (mod (p*x) m) x (pred b)


-- Checking if a = b (mod m) is congruent, i.e. a mod m = b
{-
	Usage:
		isCongruent 132 2 130
		>>> True
		
		isCongruent 13493 238 234
		>>> False
-}
isCongruent a b m = (a `mod` m) == b


-- Finding congruent pair
-- linear congruence formula: ax = b (mod m). The objective is to find x (within 'limit') such that ax mod m = b when a, b and m are given.
{-
	Usage:
		findCongruentPair 5 6 199 100
		>>> [41]
		
		findCongruentPair 5 6 199 500
		>>> [41,240,439]
-}
findCongruentPair a b m limit = [ x | x <- [0..limit], (modMult a x m) == b ]


-- Find congruent pair (sum)
-- a + x = b (mod m)
{-
	Usage:
		findCongruentPair1 10 4 5 100
		>>> [4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99]
		
		findCongruentPair1 113 20 40 100
		>>> [27,67]
-}
findCongruentPair1 a b m limit = [ x | x <- [0..limit], ((a+x) `mod` m) == b ]
