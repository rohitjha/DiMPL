{-|
Module      : Relation
Description : Relation module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for
    * Generating element set
    * Obtaining list of first element values
    * Obtaining list of second element values
    * Obtaining list of first element values for a specified element as a second element
    * Obtaining list of second element values for a specified element as a first element
    * Checking for reflexivity, symmetricity, anti-symmetricity, transitivity
    * Union, intersection and difference of two relations
    * Relation composition
    * Power of relations
    * Reflexive, Symmetric and Transitive closures
-}

module Relation
(
    Relation(..),
    relationToList,
    listToRelation,
    inverse,
    getDomain,
    getRange,
    elements,
    returnDomainElems,
    returnRangeElems,
    isReflexive,
    isIrreflexive,
    isSymmetric,
    isAsymmetric,
    isAntiSymmetric,
    isTransitive,
    rUnion,
    rUnionL,
    rIntersection,
    rIntersectionL,
    rDifference,
    composite,
    rPower,
    reflClosure,
    symmClosure,
    tranClosure,
    isEquivalent,
    isWeakPartialOrder,
    isWeakTotalOrder,
    isStrictPartialOrder,
    isStrictTotalOrder
)
where


import qualified Data.List as L


{-|
    The 'Relation' data type is used for represnting relations (discrete mathematics).

    For example:

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> r2
    {(1,1),(2,2),(3,3)}
-}
newtype Relation a = Relation [(a,a)] deriving (Eq)

instance (Show a) => Show (Relation a) where
    showsPrec _ (Relation s) = showRelation s

showRelation [] str = showString "{}" str
showRelation (x:xs) str = showChar '{' (shows x (showl xs str))
    where 
        showl [] str = showChar '}' str
        showl (x:xs) str = showChar ',' (shows x (showl xs str))


{-|
    The 'relationToList' function converts a 'Relation' to a list representation.

    For example:

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> relationToList r2
    [(1,1),(2,2),(3,3)]
-}
relationToList :: Relation t -> [(t, t)]
relationToList (Relation r) = r


{-|
    The 'listToRelation' function converts a list to a relation.

    For example:

    >>> let l = [(1,2),(2,3),(1,3)]
    >>> let r = listToRelation l
    >>> r
    {(1,2),(2,3),(1,3)}
-}
listToRelation :: [(a, a)] -> Relation a
listToRelation r = Relation r


{-|
    The 'inverse' function returns the inverse 'Relation' of a specified 'Relation'.

    For example:

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> inverse r1
    {(1,1),(2,1),(3,1),(1,2),(2,2),(3,2),(1,3),(2,3),(3,3)}

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> inverse r2
    {(1,1),(2,2),(3,3)}
-}
inverse :: Relation a -> Relation a
inverse (Relation a) = listToRelation [ (y,x) | (x,y) <- a ]


{-|
    The 'getDomain' function returns the list of all "a" where (a,b) <- 'Relation'.

    For example:
        
    >>> getDomain (Relation [(1,2),(3,4),(2,5)])
    [1,3,2]

    >>> getDomain (Relation [])
    []
-}
getDomain :: Eq a => Relation a -> [a]
getDomain (Relation r) = L.nub [fst x | x <- r]


{-|
    The 'getRange' function returns the list of all "b" where (a,b) <- 'Relation'.

    For example:
        
    >>> getRange (Relation [(1,2),(3,4),(2,5)])
    [2,4,5]

    >>> getRange (Relation [])
    []
-}
getRange :: Eq a => Relation a -> [a]
getRange (Relation r) = L.nub [snd x | x <- r]


{-|
    The 'elements' function returns a list of all elements in a 'Relation'.

    For example:

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> elements r1
    [1,2,3]
-}
elements :: Eq a => Relation a -> [a]
elements (Relation r) = getDomain (Relation r) `L.union` getRange (Relation r)


{-|
    The 'returnFirstElems' function returns alist of all "a" where (a,b) <- 'Relation' and "b" is specified

    For example:

    >>> returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 1
    []

    >>> returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 4
    [3]

    >>> returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 3
    [1,2,3]
-}
returnDomainElems :: Eq a => Relation a -> a -> [a]
returnDomainElems (Relation r) x = L.nub [a | a <- getDomain (Relation r), (a,x) `elem` r]


{-|
    The 'returnSecondElems' function returns list of all 'b' where (a,b) <- Relation and 'a' is specified/

    For example:
    
    >>> returnSecondElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 3
    [3,4]

    >>> returnSecondElems (Relation [(1,2),(1,3),(2,5)]) 1
    [2,3]
-}
returnRangeElems :: Eq a => Relation a -> a -> [a]
returnRangeElems (Relation r) x = L.nub [b | b <- getRange (Relation r), (x,b) `elem` r]


{-|
    The 'isReflexive' function checks if a 'Relation' is reflexive or not.

    For example:

    >>> isReflexive (Relation [(1,1),(1,2),(2,2),(2,3)])
    False

    >>> isReflexive (Relation [(1,1),(1,2),(2,2)])
    True
-}
isReflexive :: Eq t => Relation t -> Bool
isReflexive (Relation r) = and [(a,a) `elem` r | a <- elements (Relation r)]


{-|
    The 'isIrreflexive' function checks if a 'Relation' is irreflexive or not.

    For example:

    >>> isIrreflexive (Relation [(1,1),(1,2),(2,2),(2,3)])
    True

    >>> isIrreflexive (Relation [(1,1),(1,2),(2,2)])
    False
-}
isIrreflexive :: Eq t => Relation t -> Bool
isIrreflexive (Relation r) = not $ isReflexive (Relation r)


{-|
    The 'isSymmetric' function checks if a 'Relation' is symmetric or not.

    For example:

    >>> isSymmetric (Relation [(1,1),(1,2),(2,2)])
    False

    >>> isSymmetric (Relation [(1,1),(1,2),(2,2),(2,1)])
    True
-}
isSymmetric :: Eq a => Relation a -> Bool
isSymmetric (Relation r) = and [(b, a) `elem` r | a <- elements (Relation r), b <- elements (Relation r), (a, b) `elem` r]


{-|
    The 'isAsymmetric' function checks if a 'Relation' is asymmetric or not.

    For example:

    >>> isAntiSymmetric (Relation [(1,2),(2,1)])
    False

    >>> isAntiSymmetric (Relation [(1,2),(1,3)])
    True
-}
isAsymmetric :: Eq t => Relation t -> Bool
isAsymmetric (Relation r) = and [ (b,a) `notElem` r | a <- elements (Relation r), b <- elements (Relation r), (a,b) `elem` r]


{-|
    The 'isAntiSymmetric' function checks if a 'Relation' is anti-symmetric or not.

    For example:

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> isAntiSymmetric r2
    True

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> isAntiSymmetric r1
    False
-}
isAntiSymmetric :: Eq a => Relation a -> Bool
isAntiSymmetric (Relation r) = and [ a == b | a <- elements (Relation r), b <- elements (Relation r), (a,b) `elem` r, (b,a) `elem` r]


{-|
    The 'isTransitive' function checks if a 'Relation' is transitive or not.

    For example:

    >>> isTransitive (Relation [(1,1),(1,2),(2,1)])
    False

    >>> isTransitive (Relation [(1,1),(1,2),(2,1),(2,2)])
    True

    >>> isTransitive (Relation [(1,1),(2,2)])
    True
-}
isTransitive :: Eq a => Relation a -> Bool
isTransitive (Relation r) = and [(a,c) `elem` r | a <- elements (Relation r), b <- elements (Relation r), c <- elements (Relation r), (a,b) `elem` r, (b,c) `elem` r]


{-|
    The 'rUnion' function returns the union of two relations.

    For example:

    >>> rUnion (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
    {(1,1),(1,2),(2,2),(2,3)}

    >>> rUnion (Relation [(1,1),(1,2)]) (Relation [(1,1)])
    {(1,1),(1,2)}
-}
rUnion :: Ord a => Relation a -> Relation a -> Relation a
rUnion (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) (r1 ++ [e | e <- r2, e `notElem` r1]))


{-|
    The 'rUnionL' function returns the union of a list of 'Relation'.

    For example:
        
    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> rUnionL [r1,r2]
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}
-}
{-rUnionL :: (Ord t1, Foldable t) => t (Relation t1) -> Relation t1-}
rUnionL :: (Ord t) => [Relation t] -> Relation t
rUnionL = foldl1 rUnion


{-|
    The 'rIntersection' function returns the intersection of two relations.

    For example:

    >>> rIntersection (Relation [(1,1),(1,2)]) (Relation [(1,1)])
    {(1,1)}

    >>> rIntersection (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
    {}
-}
rIntersection :: Ord a => Relation a -> Relation a -> Relation a
rIntersection (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) [e | e <- r1, e `elem` r2])


{-|
    The 'rIntersectionL' function returns the intersection of a list of 'Relation'.

    For example:

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> rIntersectionL [r1,r2]
    {(1,1),(2,2),(3,3)}
-}
{-rIntersectionL :: (Ord a, Foldable t) => t (Relation a) -> Relation a-}
rIntersectionL :: (Ord a) => [Relation a] -> Relation a
rIntersectionL = foldl1 rIntersection


{-|
    The 'rDifference' function returns the difference of two relations.

    For example:

    >>> rDifference (Relation [(1,1),(1,2)]) (Relation [(1,1)])
    {(1,2)}

    >>> rDifference (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
    {(1,1),(1,2)}
-}
rDifference :: Ord a => Relation a -> Relation a -> Relation a
rDifference (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) [e | e <- r1, e `notElem` r2])


{-|
    The 'composite' function returns the composite / concatenation of two relations.

    For example:

    >>> composite (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
    {(1,2),(1,3)}

    >>> composite (Relation [(1,1),(1,2)]) (Relation [(1,1)])
    {(1,1)}
-}
composite :: Eq a => Relation a -> Relation a -> Relation a
composite (Relation r1) (Relation r2) = Relation $ L.nub [(a,c) | a <- elements (Relation r1), b <- elements (Relation r1), b <- elements (Relation r2), c <- elements (Relation r2), (a,b) `elem` r1, (b,c) `elem` r2]


{-|
    The 'rPower' function returns the power of a 'Relation'.

    For example:

    >>> let r4 = Relation [(1,2), (2,3), (2,4), (3,3)]
    >>> r4
    {(1,2),(2,3),(2,4),(3,3)}

    >>> rPower r4 2
    {(1,3),(1,4),(2,3),(3,3)}

    >>> rPower r4 (-2)
    {(3,1),(3,2),(3,3),(4,1)}
    --| pow < 0 = rPower (Relation [(b, a) | (a, b) <- r]) (-pow)
-}
rPower :: (Eq a, Eq a1, Integral a1) => Relation a -> a1 -> Relation a
rPower (Relation r) pow
    | pow < 0 = rPower (inverse (Relation r)) (-pow)
    | pow == 1 = Relation r
    | otherwise = composite (rPower (Relation r) (pow - 1)) (Relation r)


{-|
    The 'reflClosure' function returns the Reflecive Closure of a 'Relation'.

    For example:

    >>> reflClosure (Relation [(1,1),(1,2),(4,5)])
    {(1,1),(1,2),(2,2),(4,4),(4,5),(5,5)}

    >>> reflClosure (Relation [(1,1),(1,3)])
    {(1,1),(1,3),(3,3)}
-}
reflClosure :: Ord a => Relation a -> Relation a
reflClosure (Relation r) = rUnion (Relation r) (delta (Relation r))
    where
        delta (Relation r) = Relation [(a,b) | a <- elements (Relation r), b <- elements (Relation r), a == b]


{-|
    The 'symmClosure' function returns the Symmetric Closure of a 'Relation'.

    For example:
    
    >>> symmClosure (Relation [(1,1),(1,2),(4,5)])
    {(1,1),(1,2),(2,1),(4,5),(5,4)}

    >>> symmClosure (Relation [(1,1),(1,3)])
    {(1,1),(1,3),(3,1)}
-}
symmClosure :: Ord a => Relation a -> Relation a
symmClosure (Relation r) = rUnion (Relation r) (rPower (Relation r) (-1))


{-|
    The 'tranClosure' function returns the Transitive Closure of a 'Relation'.

    For example:

    >>> tranClosure (Relation [(1,1),(1,2),(2,1)])
    {(1,1),(1,2),(2,1),(2,2)}

    >>> tranClosure (Relation [(1,1),(1,2),(1,3),(2,2),(3,1),(3,2)])
    {(1,1),(1,2),(1,3),(2,2),(3,1),(3,2),(3,3)}
-}
tranClosure :: Ord a => Relation a -> Relation a
tranClosure (Relation r) = foldl1 rUnion [ rPower (Relation r) n | n <- [1 .. length (elements (Relation r)) ]]


{-|
    The 'isEquivalent' function checks if a 'Relation' is equivalent (reflexive, symmetric and transitive).

    For example:

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> isEquivalent r2
    True

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> isEquivalent r1
    True

    >>> isEquivalent (Relation [(1,2), (2,3)])
    False
-}
isEquivalent :: Eq a => Relation a -> Bool
isEquivalent (Relation r) = isReflexive (Relation r) && isSymmetric (Relation r) && isTransitive (Relation r)


{-|
    The 'isWeakPartialOrder' function checks if a 'Relation' is a weak partial order (reflexive, anti-symmetric and transitive).

    For example:

    >>> r1
    {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

    >>> isWeakPartialOrder r1
    False

    >>> r2
    {(1,1),(2,2),(3,3)}

    >>> isWeakPartialOrder r2
    True
-}
isWeakPartialOrder :: Eq a => Relation a -> Bool
isWeakPartialOrder (Relation r) = isReflexive (Relation r) && isAntiSymmetric (Relation r) && isTransitive (Relation r)

{-|
    The 'isWeakTotalOrder' function checks if a 'Relation' is a Weak Total Order, i.e. it is a Weak Partial Order and for all "a" and "b" in 'Relation' "r", (a,b) or (b,a) are elements of r.
-}
isWeakTotalOrder :: Eq a => Relation a -> Bool
isWeakTotalOrder (Relation r) = isWeakPartialOrder (Relation r) && and [ ((a,b) `elem` r) || ((b,a) `elem` r) | a <- elements (Relation r), b <- elements (Relation r) ]


{-|
    The 'isStrictPartialOrder' function checks if a 'Relation' is a Strict Partial Order, i.e. it is irreflexive, asymmetric and transitive.
-}
isStrictPartialOrder :: Eq a => Relation a -> Bool
isStrictPartialOrder (Relation r) = isIrreflexive (Relation r) && isAsymmetric (Relation r) && isTransitive (Relation r)


{-|
    The 'isStrictTotalOrder' function checks if a 'Relation' is a Strict Total Order, i.e. it is a Strict Partial Order, and for all "a" and "b" in 'Relation' "r", either (a,b) or (b,a) are elements of r or a == b.
-}
isStrictTotalOrder :: Eq a => Relation a -> Bool
isStrictTotalOrder (Relation r) = isStrictPartialOrder (Relation r) && and [ ((a,b) `elem` r) || ((b,a) `elem` r) || a==b | a <- elements (Relation r), b <- elements (Relation r) ]


-- SAMPLE RELATIONS --
r1 = Relation [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

r2 = Relation [(1,1),(2,2),(3,3)]

r3 = Relation []
