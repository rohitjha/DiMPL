{-|
Module      : Logic
Description : Mathematical logic module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
  * AND
  * OR
  * NOT
  * XOR
  * NAND
  * NOR
  * Logical Implication
  * Logical Equality
  * Operators for the above
  * Operations on list for the above
-}

module Logic
(
  and',
  or',
  xor,
  xnor,
  nand,
  nor,
  equals,
  implies,
  (/\),
  (\/),
  (==>),
  (<=>),
  notL,
  andL,
  orL,
  xorL,
  xnorL,
  nandL,
  norL
)
where


{-|
    The 'and'' function is for the binary AND operation.

    For example:
    
    >>> let a = True
    >>> let b = False
    >>> c = and' a b
    c = False

    >>> True `and'` False
    False
-}
and' :: Bool -> Bool -> Bool
and' a b = a && b


{-|
    The 'or'' function is for the binary OR operation.
    
    For example:
    
    >>> let a = True
    >>> let b = False
    >>> let c = or a b
    c = True
    
    >>> True `or'` False
    True
-}
or' :: Bool -> Bool -> Bool
or' a b = a || b


{-|
    The 'xor' function is for the binary XOR operation.

    For example:
        
    >>> let a = True
    >>> let b = False
    >>> let c = xor a b
    c = True
    
    >>> True `xor` False
    True
-}
xor :: Bool -> Bool -> Bool
xor a b
  | a == b = False
  | otherwise = True


{-|
    The 'xnor' function is for the binary XNOR operation.

    For example:
        
    >>> xnor True False
    False
    
    >>> xnor True True
    True
    
    >>> xnor False True
    False
    
    >>> False `xnor` False
    True
-}
xnor :: Bool -> Bool -> Bool
xnor a b = not (xor a b)


{-|
    The 'nand' function is for the binary NAND operation.

    For example:

    >>> let a = True
    >>> let b = False    
    >>> let c = nand a b
    c = True
    
    >>> True `nand` False
    True
-}
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)


{-|
    The 'nor' function is for the binary NOR operation.

    For example:

    >>> let a = True
    >>> let b = False
    >>> let c = nor a b
    c = False
    
    >>> True `nor` False
    False
-}
nor :: Bool -> Bool -> Bool
nor a b = not (a || b)


{-|
    The 'equals' function checks for binary logical equality.

    For example:

    >>> let a = True
    >>> let b = False
    >>> let c = equals a b
    c = False
    
    >>> True `equals` True
    True
-}
equals :: Bool -> Bool -> Bool
equals a b = a == b


{-|
    The 'implies' function checks for binart logical implication.

    For example:

    >>> let a = True
    >>> let b = False
    >>> let c = implies a b
    c = False
    
    >>> True `implies` False
    False
-}
implies :: Bool -> Bool -> Bool
implies a b
  | a && not b = False
  | otherwise = True


{-|
    The '/\' operator is the binary AND operator.

    For example:
        
    >>> True /\ True
    True
    
    >>> True /\ False
    False
    
    >>> False /\ True
    False
    
    >>> False /\ False
    False
-}
(/\) :: Bool -> Bool -> Bool
a /\ b = a && b


{-|
    The '\/' operator is the binary OR operator.

    For example:
        
    >>> True \/ True
    True
    
    >>> True \/ False
    True
    
    >>> False \/ True
    True
    
    >>> False \/ False
    False
-}
(\/) :: Bool -> Bool -> Bool
a \/ b = a || b


{-|
    The '==>' operator is the binary implication operator.
    This operator can be used instead of the 'implies' function.

    For example:
    
    >>> True ==> False
    False
    
    >>> False ==> False
    True
    
    >>> False ==> True
    True
    
    >>> True ==> True
    True
-}
(==>) :: Bool -> Bool -> Bool
a ==> b = implies a b


{-|
    The '<=>' operator is the binary equality operator.

    For example:
        
    >>> True <=> True
    True
    
    >>> True <=> False
    False
    
    >>> False <=> True
    False
    
    >>> False <=> False
    True
-}
(<=>) :: Bool -> Bool -> Bool
a <=> b = a == b


{-|
    The 'notL' function is a unary NOT function that is applied to each element of a Bool list.

    For example:

    >>> notL [True, True, False]
    [False,False,True]
    
    >>> notL [True, False, True, True, False]
    [False,True,False,False,True]
    
    >>> notL [True]
    [False]
    
    >>> notL []
    []
-}
notL :: [Bool] -> [Bool]
notL = map not


{-|
    The 'andL' function is an AND operator that is applied on all elements of a Bool list.

    For example:
    
    >>> andL [True, False, True, True, False]
    False
    
    >>> andL [True, True]
    True
    
    >>> andL [False]
    False
-}
andL :: [Bool] -> Bool
andL = and


{-|
    The 'orL' function is an OR operator that is applied on all elements of a Bool list.

    For example:
    
    >>> orL [True, False, True, True, False]
    True
    
    >>> orL [True, False]
    True
    
    >>> orL [False, False]
    False
-}
orL :: [Bool] -> Bool
orL = or


{-|
    The 'xorL' function is an XOR operator that is applied on all the elements of a Bool list.

    For example:
    
    >>> xorL [True, False, True, True, False]
    True
    
    >>> xorL [False, False]
    False
-}
xorL :: [Bool] -> Bool
xorL = foldl1 xor


{-|
    The 'nandL' function is a NAND operator that is applied on all the elements of a Bool list.

    For example:
    
    >>> nandL [True, False, True, True, False]
    True
    
    >>> nandL [False, False]
    True
-}
nandL :: [Bool] -> Bool
nandL = foldl1 nand


{-|
    The 'norL' function is a NOR operator that is applied on all the elements of a Bool list.

    For example:

    >>> norL [True, False, True, True, False]
    True
    
    >>> norL [False, False]
    True
-}
norL :: [Bool] -> Bool
norL = foldl1 nor


{-|
    The 'xnorL' function is an XNOR operator that is applied on all the elements of a Bool list.

    For example:

    >>> xnorL [True, False, True, True, False]
    True
    
    >>> xnorL [False, False]
    True
-}
xnorL :: [Bool] -> Bool
xnorL = foldl1 xnor
