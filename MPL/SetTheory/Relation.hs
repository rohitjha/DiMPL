{-
-----------------------------------
| Module for Relations in MPL |
-----------------------------------

Functionality for
	-> Generating element set
	-> Obtaining list of first element values
	-> Obtaining list of second element values
	-> Obtaining list of first element values for a specified element as a second element
	-> Obtaining list of second element values for a specified element as a first element
	-> Checking for reflexivity, symmetricity, anti-symmetricity, transitivity
	-> Union, intersection and difference of two relations
	-> Relation composition
	-> Power of relations
	-> Reflexive, Symmetric and Transitive closures

Author: Rohit Jha, Alfy Samuel, Ashmee Pawar
Version: 0.1
Date: 25 Feb 2013
-}

module MPL.SetTheory.Relation
(
	Relation(..),
	relation2list,
	getFirst,
	getSecond,
	elemSet,
	returnFirstElems,
	returnSecondElems,
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
	rComposite,
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


-- Relation data type
newtype Relation a = Relation [(a,a)] deriving (Eq)

instance (Show a) => Show (Relation a) where
	showsPrec _ (Relation s) str = showRelation s str

showRelation [] str = showString "{}" str
showRelation (x:xs) str = showChar '{' (shows x (showl xs str))
	where 
		showl [] str = showChar '}' str
		showl (x:xs) str = showChar ',' (shows x (showl xs str))


-- Converting a relation to list
relation2list (Relation r) = r


elemSet r = L.union (getFirst (Relation r)) (getSecond (Relation r))


-- Returns list of all 'a' where (a,b) <- Relation
{-
	Usage:
		getFirst (Relation [(1,2),(3,4),(2,5)])
		>>> [1,3,2]

		getFirst (Relation [])
		>>> []
-}
getFirst (Relation r) = L.nub [fst x | x <- r]


-- Returns list of all 'b' where (a,b) <- Relation
{-
	Usage:
		getSecond (Relation [(1,2),(3,4),(2,5)])
		>>> [2,4,5]

		getSecond (Relation [])
		>>> []
-}
getSecond (Relation r) = L.nub [snd x | x <- r]


-- Returns list of all 'a' where (a,b) <- Relation and 'b' is specified
{-
	Usage:
		returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 1
		>>> []

		returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 4
		>>> [3]

		returnFirstElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 3
		>>> [1,2,3]
-}
returnFirstElems (Relation r) x = L.nub [fst (a,x) | a <- getFirst (Relation r), (a,x) `elem` r]


-- Returns list of all 'b' where (a,b) <- Relation and 'a' is specified
{-
	Usage:
		returnSecondElems (Relation [(1,2),(1,3),(2,3),(3,3),(3,4)]) 3
		>>> [3,4]

		returnSecondElems (Relation [(1,2),(1,3),(2,5)]) 1
		>>> [2,3]
-}
returnSecondElems (Relation r) x = L.nub [snd (x,b) | b <- getSecond (Relation r), (x,b) `elem` r]


-- Checks if a relation is reflexive or not
{-
	Usage:
		isReflexive (Relation [(1,1),(1,2),(2,2),(2,3)])
		>>> False

		isReflexive (Relation [(1,1),(1,2),(2,2)])
		>>> True
-}
isReflexive (Relation r) = and [(a,a) `elem` r | a <- elemSet r]


isIrreflexive (Relation r) = not $ isReflexive (Relation r)


-- Checks if a relation is symmetric or not
{-
	Usage:
		isSymmetric (Relation [(1,1),(1,2),(2,2)])
		>>> False

		isSymmetric (Relation [(1,1),(1,2),(2,2),(2,1)])
		>>> True
-}
isSymmetric (Relation r) = and [((b,a) `elem` r) | a <- elemSet r, b <- elemSet r, (a,b) `elem` r]


-- Checks if a relation is asymmetric or not
{-
	Usage:
		isAntiSymmetric (Relation [(1,2),(2,1)])
		>>> False

		isAntiSymmetric (Relation [(1,2),(1,3)])
		>>> True
-}
isAsymmetric (Relation r) = and [not ((b,a) `elem` r) | a <- elemSet r, b <- elemSet r, (a,b) `elem` r]


isAntiSymmetric (Relation r) = and [ a==b | a <- elemSet r, b <- elemSet r, (a,b) `elem` r, (b,a) `elem` r]


-- Checks if a relation is transitive or not
{-
	Usage:
		isTransitive (Relation [(1,1),(1,2),(2,1)])
		>>> False

		isTransitive (Relation [(1,1),(1,2),(2,1),(2,2)])
		>>> True

		isTransitive (Relation [(1,1),(2,2)])
		>>> True
-}
isTransitive (Relation r) = and [(a,c) `elem` r | a <- elemSet r, b <- elemSet r, c <- elemSet r, (a,b) `elem` r, (b,c) `elem` r]


-- Returns union of two relations
{-
	Usage:
		rUnion (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
		>>> {(1,1),(1,2),(2,2),(2,3)}

		rUnion (Relation [(1,1),(1,2)]) (Relation [(1,1)])
		>>> {(1,1),(1,2)}
-}
rUnion (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) (r1 ++ [e | e <- r2, not (elem e r1)]))


-- Returns union of list of relations
{-
	Usage:
		r1
		>>> {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

		r2
		>>> {(1,1),(2,2),(3,3)}

		rUnionL [r1,r2]
		>>> {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}
-}
rUnionL r = foldl1 (rUnion) r


-- Returns intersection of two relations
{-
	Usage:
		rIntersection (Relation [(1,1),(1,2)]) (Relation [(1,1)])
		>>> {(1,1)}

		rIntersection (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
		>>> {}
-}
rIntersection (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) [e | e <- r1, (elem e r2)])


-- Returns intersection of a list of relations
{-
	Usage:
		r1
		>>> {(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)}

		r2
		>>> {(1,1),(2,2),(3,3)}

		rIntersectionL [r1,r2]
		>>> {(1,1),(2,2),(3,3)}
-}
rIntersectionL r = foldl1 (rIntersection) r


-- Returns difference of two relations
{-
	Usage:
		rDifference (Relation [(1,1),(1,2)]) (Relation [(1,1)])
		>>> {(1,2)}

		rDifference (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
		>>> {(1,1),(1,2)}
-}
rDifference (Relation r1) (Relation r2) = Relation ((L.sort . L.nub) [e | e <- r1, not (elem e r2)])


-- Returns composite of two relations
{-
	Usage:
		rComposite (Relation [(1,1),(1,2)]) (Relation [(2,3),(2,2)])
		>>> {(1,2),(1,3)}

		rComposite (Relation [(1,1),(1,2)]) (Relation [(1,1)])
		>>> {(1,1)}
-}
rComposite (Relation r1) (Relation r2) = Relation $ L.nub [(a,c) | a <- elemSet r1, b <- elemSet r1, b <- elemSet r2, c <- elemSet r2, (a,b) `elem` r1, (b,c) `elem` r2]


-- Returns power of a relation
{-
	Usage:
		rPower (Relation [(1,1),(1,2),(4,5)]) 3
		>>> {(1,1),(1,2)}

		rPower (Relation [(1,1),(1,2),(2,4),(4,5)]) 3
		>>> {(1,1),(1,2),(1,4),(1,5)}
-}
rPower (Relation r) pow =
	if (pow == (-1))
	then Relation [(b,a) | (a,b) <- r]
	else
		if (pow == 1)
		then (Relation r)
		else rComposite (rPower (Relation r) (pow-1)) (Relation r)


-- Reflexive closure
{-
	Usage:
		reflClosure (Relation [(1,1),(1,2),(4,5)])
		>>> {(1,1),(1,2),(2,2),(4,4),(4,5),(5,5)}

		reflClosure (Relation [(1,1),(1,3)])
		>>> {(1,1),(1,3),(3,3)}
-}
reflClosure (Relation r) = rUnion (Relation r) (delta (Relation r))
	where
		delta (Relation r) = Relation [(a,b) | a <- elemSet r, b <- elemSet r, a == b]


-- Symmetric closure
{-
	Usage:
		symmClosure (Relation [(1,1),(1,2),(4,5)])
		>>> {(1,1),(1,2),(2,1),(4,5),(5,4)}

		symmClosure (Relation [(1,1),(1,3)])
		>>> {(1,1),(1,3),(3,1)}
-}
symmClosure (Relation r) = rUnion (Relation r) (rPower (Relation r) (-1))


-- Transitive closure
{-
	Usage:
		tranClosure (Relation [(1,1),(1,2),(2,1)])
		>>> {(1,1),(1,2),(2,1),(2,2)}

		tranClosure (Relation [(1,1),(1,2),(1,3),(2,2),(3,1),(3,2)])
		>>> {(1,1),(1,2),(1,3),(2,2),(3,1),(3,2),(3,3)}
-}
tranClosure (Relation r) = foldl1 (rUnion) [ (rPower (Relation r) n) | n <- [1 .. length (elemSet r) ]]


isEquivalent (Relation r) = isReflexive (Relation r) && isSymmetric (Relation r) && isTransitive (Relation r)


isWeakPartialOrder (Relation r) = isReflexive (Relation r) && isAntiSymmetric (Relation r) && isTransitive (Relation r)


isWeakTotalOrder (Relation r) = isWeakPartialOrder (Relation r) && (and [ ((a,b) `elem` r) || ((b,a) `elem` r) | a <- elemSet r, b <- elemSet r ] )


isStrictPartialOrder (Relation r) = isIrreflexive (Relation r) && isAsymmetric (Relation r) && isTransitive (Relation r)


isStrictTotalOrder (Relation r) = isStrictPartialOrder (Relation r) && (and [ ((a,b) `elem` r) || ((b,a) `elem` r) || a==b | a <- elemSet r, b <- elemSet r ] )


-- SAMPLE RELATIONS --
r1 = Relation [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

r2 = Relation [(1,1),(2,2),(3,3)]

r3 = Relation []
