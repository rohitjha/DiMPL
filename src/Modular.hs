{-|
Module      : Modular
Description : Modular Arithmetic module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
    * Modular addition
    * Modular subtraction
    * Modular multiplication
    * Modular exponentiation
    * Checking congruence
    * Solving linear congruences
-}


module Modular
(
    modAdd,
    modSub,
    modMult,
    modExp,
    isCongruent,
    findCongruentPair,
    findCongruentPair'
)
where


{-|
    The 'modAdd' function performs addition using modular arithmetic.
    
    prop> modAdd a b m = (a + b) mod m

    prop> (a + b) mod m = (a mod m) + (b mod m)

    For example:
    
    >>> modAdd 17 44 11
    6
    
    >>> modAdd 126832 1832 11
    8
-}
modAdd :: Integral a => a -> a -> a -> a
modAdd a b m = (a `mod` m) + (b `mod` m)


{-|
    The 'modSub' function performs subtraction using modular arithmetic.
    
    prop> modSub a b m = (a - b) mod m

    prop> (a - b) mod m = (a mod m) - (b mod m)

    For example:

    >>> modSub 117 14 11
    4
    
    >>> modSub 114787 23934 3874
    1751
-}
modSub :: Integral a => a -> a -> a -> a
modSub a b m = (a `mod` m) - (b `mod` m)


{-|
    The 'modMult' function performs multiplication using modular arithmetic.
    
    prop> modMult a b m = (a * b) mod m

    prop> (a * b) mod m = ((a mod m) * (b mod m)) mod m

    For example:

    >>> modMult 117 14 11
    10
    
    >>> modMult 114787 23934 3874
    2974
-}
modMult :: Integral a => a -> a -> a -> a
modMult a b m = ( (a `mod` m) * (b `mod` m) ) `mod` m


{-|
    The 'modExp' function is for the Modular Exponentiation operation (a ^ b mod m)
    
    For example:

    >>> modExp 12 5 6
    0
    
    >>> modExp 112 34 546
    532
    
    >>> modExp 515 5151 1563
    1004
-}
modExp :: (Integral a, Integral a1) => a -> a1 -> a -> a
modExp a b m = modexp' 1 a b
    where
        modexp' p _ 0 = p
        modexp' p x b =
            if even b
                then modexp' p (mod (x*x) m) (div b 2)
            else modexp' (mod (p*x) m) x (pred b)


{-|
    The 'isCongruent' function checks for modular congruency.
    For a = b (mod m) to be congruent, a mod m = b.

    For example:

    >>> isCongruent 132 2 130
    True
    
    >>> isCongruent 13493 238 234
    False
-}
isCongruent :: Integral a => a -> a -> a -> Bool
isCongruent a b m = (a `mod` m) == b


{-|
    The 'findCongruentPair' function uses the linear congruence formula a * x = b (mod m) or (a * x) mod m = b to find x, when a, b and m are given.
    
    For example:
        
    >>> findCongruentPair 5 6 199 100
    [41]

    >>> isCongruent (5 * 41) 6 199
    True
    
    >>> findCongruentPair 5 6 199 500
    [41,240,439]
-}
findCongruentPair :: Integral t => t -> t -> t -> t -> [t]
findCongruentPair a b m limit = [ x | x <- [0..limit], modMult a x m == b ]


{-|
    The 'findCongruentPair'' function uses the linear congruence formula a + b = b (mod m) or (a + x) mod m = b to find x, when a, b and m are given.

    For example:

    >>> findCongruentPair' 10 4 5 100
    [4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99]
    
    >>> findCongruentPair' 113 20 40 100
    [27,67]

    >>> isCongruent (113 + 27) 20 40
    True

    >>> isCongruent (113 + 67) 20 40
    True
-}
findCongruentPair' :: Integral t => t -> t -> t -> t -> [t]
findCongruentPair' a b m limit = [ x | x <- [0..limit], ((a+x) `mod` m) == b ]
