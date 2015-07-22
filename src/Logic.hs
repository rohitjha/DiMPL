{-
------------------------
| Logic Module for MPL |
------------------------

Functionality for:
  -> AND
  -> OR
  -> NOT
  -> XOR
  -> NAND
  -> NOR
  -> Logical Implication
  -> Logical Equality
  -> Operators for the above
  -> Operations on list for the above

Author: Rohit Jha
Version: 0.1
Date: 7 Feb 2013
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


-- Binary AND Function
{-
    Usage:
        let a = True
        let b = False

        c = and' a b
        >>> c = False

        d = a `and'` b
        >>> d = False
-}
and' :: Bool -> Bool -> Bool
and' a b = a && b


-- Binary OR Function
{-
    Usage:
        let a = True
        let b = False
        
        let c = or a b
        >>> c = True
        
        let d = a `or'` b
        >>> d = True
-}
or' :: Bool -> Bool -> Bool
or' a b = a || b


-- Unary NOT Function
-- Use 'not' from Prelude
{-
    Usage:
        let b = not True
        >>> b = False
-}


-- Binary XOR Function
{-
    Usage:
        let a = True
        let b = False
        
        let c = xor a b
        >>> c = True
        
        let d = a `xor` b
        >>> d = True
-}
xor :: Bool -> Bool -> Bool
xor a b
  | a == b = False
  | otherwise = True


-- Binary XNOR Funtion
{-
    Usage:
        xnor True False
        >>> False
        
        xnor True True
        >>> True
        
        xnor False True
        >>> False
        
        xnor False False
        >>> True
-}
xnor :: Bool -> Bool -> Bool
xnor a b = not (xor a b)


-- Binary NAND Function
{-
    Usage:
        let a = True
        let b = False
        
        let c = nand a b
        >>> c = True
        
        let d = a `nand` b
        >>> d = True
-}
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)


-- Binary NOR Function
{-
    Usage:
        let a = True
        let b = False
        
        let c = nor a b
        >>> c = False
        
        let d = a `nor` b
        >>> d = False
-}
nor :: Bool -> Bool -> Bool
nor a b = not (a || b)


-- Binary Logical Equality
{-
    Usage:
        let a = True
        let b = False
        
        let c = equals a b
        >>> c = False
        
        let d = a `equals` b
        >>> d = False
-}
equals :: Bool -> Bool -> Bool
equals a b = a == b


-- Binary Logical Implication
{-
    Usage:
        let a = True
        let b = False
        
        let c = implies a b
        >>> c = False
        
        let d = a `implies` b
        >>> d = False
-}
implies :: Bool -> Bool -> Bool
implies a b
  | a && not b = False
  | otherwise = True


-- Binary and Operator
{-
    Usage:
        True /\ True
        >>> True
        
        True /\ False
        >>> False
        
        False /\ True
        >>> False
        
        False /\ False
        >>> False
-}
(/\) :: Bool -> Bool -> Bool
a /\ b = a && b


-- Binary or Operator
{-
    Usage:
        True \/ True
        >>> True
        
        True \/ False
        >>> True
        
        False \/ True
        >>> True
        
        False \/ False
        >>> False
-}
(\/) :: Bool -> Bool -> Bool
a \/ b = a || b


-- Binary implication Operator
{-
    Usage:
        True ==> False
        >>> False
        
        False ==> False
        >>> True
        
        False ==> True
        >>> True
        
        True ==> True
        >>> True
-}
(==>) :: Bool -> Bool -> Bool
a ==> b = implies a b


-- Binary equality Operator
{-
    Usage:
        True <=> True
        >>> True
        
        True <=> False
        >>> False
        
        False <=> True
        >>> False
        
        False <=> False
        >>> True
-}
(<=>) :: Bool -> Bool -> Bool
a <=> b = a == b


-- unary not Operator on a list of Bool
{-
    Usage:
        notL [True, True, False]
        >>> [False,False,True]
        
        notL [True, False, True, True, False]
        >>> [False,True,False,False,True]
        
        notL [True]
        >>> [False]
        
        notL []
        >>> []
-}
notL :: [Bool] -> [Bool]
notL = map not


-- Binary and Operator on a list of Bool
{-
    Usage:
        andL [True, False, True, True, False]
        >>> False
        
        andL [True, True]
        >>> True
        
        andL [False]
        >>> False
-}
andL :: [Bool] -> Bool
andL = and


-- Binary or Operator on a list of Bool
{-
    Usage:
        orL [True, False, True, True, False]
        >>> True
        
        orL [True, False]
        >>> True
        
        orL [False, False]
        >>> False
-}
orL :: [Bool] -> Bool
orL = or


-- Binary xor Operator on a list of Bool
{-
    Usage:
        xorL [True, False, True, True, False]
        >>> True
        
        xorL [False, False]
        >>> False
-}
xorL :: [Bool] -> Bool
xorL = foldl1 xor


-- Binary nand Operator on a list of Bool
{-
    Usage:
        nandL [True, False, True, True, False]
        >>> True
        
        nandL [False, False]
        >>> True
-}
nandL :: [Bool] -> Bool
nandL = foldl1 nand


-- Binary nor Operator on a list of Bool
{-
    Usage:
        norL [True, False, True, True, False]
        >>> True
        
        norL [False, False]
        >>> True
-}
norL :: [Bool] -> Bool
norL = foldl1 nor


-- Binary xnor Operator on a list of Bool
{-
    Usage:
        xnorL [True, False, True, True, False]
        >>> True
        
        xnorL [False, False]
        >>> True
-}
xnorL :: [Bool] -> Bool
xnorL = foldl1 xnor
