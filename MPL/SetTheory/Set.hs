{-
----------------------
| Set Module for MPL |
----------------------

Functionality for:
	-> Union
	-> Intersection
	-> Difference
	-> Membership
	-> Cardinality
	-> Null set verification
	-> Subset verification
	-> Superset verification
	-> Power set generation
	-> Cartesian product
	-> Set equality
	-> Set disjoint verification

Author: Rohit Jha
Version: 0.1
Date: 5 Mar 2013
-}

module MPL.SetTheory.Set 
(
	Set(..),
	set2list,
	union,
	unionL,
	intersection,
	intersectionL,
	difference,
	isMemberOf,
	cardinality,
	isNullSet,
	isSubset,
	isSuperset,
	powerSet,
	cartProduct,
	disjoint,
	disjointL,
	natural,
	natural',
	whole,
	whole',
	sMap
)
where


import qualified Data.List as L


newtype Set a = Set [a] deriving (Ord)

instance Eq a => Eq (Set a) where
	set1 == set2 = subSet set1 set2 && subSet set2 set1


instance (Show a) => Show (Set a) where
	showsPrec _ (Set s) str = showSet s str


showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
	where 
		showl [] str = showChar '}' str
		showl (x:xs) str = showChar ',' (shows x (showl xs str))


subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = (isMemberOf x set) && subSet (Set xs) set


type Element a = a


list2set :: Ord a => [a] -> Set a
list2set [] = (Set [])
list2set list = (Set ((L.sort . L.nub) [x | x <- list]))


set2list :: Set a -> [a]
set2list (Set s) = s


-- Union of Sets
{-
	Usage:
		union (Set [2,4,6]) (Set [1,2,3])
		>>> {1,2,3,4,6}
		
		union (Set ["a", "b"]) (Set ["b", "c", "d"])
		>>> {"a","b","c","d"}
		
		union (Set [2,4..10]) (Set [1,3..10])
		>>> {1,2,3,4,5,6,7,8,9,10}
		
		union (Set "hdbshv") (Set "dsjnru")
		>>> {'b','d','h','j','n','r','s','u','v'}
-}
union :: Ord a => Set a -> Set a -> Set a
union (Set set1) (Set set2)
	= Set ((L.sort . L.nub) (set1 ++ [e | e <- set2, not (elem e set1)]))


-- Union of a list of Sets
{-
	Usage:
		s1
		>>> {1,3,5,7,9}
		
		s2
		>>> {2,4,6,8,10}

		s3
		>>> {}

		unionL [s1,s2,s3]
		>>> {1,2,3,4,5,6,7,8,9,10}
-}
unionL :: Ord a => [Set a] -> Set a
unionL s = foldl1 (union) s


-- Intersection of Sets
{-
	Usage:
		intersection (Set [2,4,6]) (Set [1,2,3])
		>>> {2}

		intersection (Set ["a", "b"]) (Set ["b", "c", "d"])
		>>> {"b"}

		intersection (Set [2,4..10]) (Set [1,3..10])
		>>> {}

		intersection (Set "hdbshv") (Set "dsjnru")
		>>> {'d','s'}
-}
intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set set1) (Set set2)
	= Set ((L.sort . L.nub) [e | e <- set1, (elem e set2)])


-- Intersection of a list of Sets
{-
	Usage:
		s4
		>>> {1,2,3,4,5,6,7,8,9,10}

		s5
		>>> {0,2,4,6,8,10}

		s6
		>>> {2,4,6,18}

		intersectionL [s4,s5,s6]
		>>> {2,4,6}
-}
intersectionL :: Ord a => [Set a] -> Set a
intersectionL s = foldl1 (intersection) s


-- Set difference
{-
	Usage:
		difference (Set [1,2,3]) (Set [2..5])
		>>> {1}

		difference (Set "irjfg") (Set "kdsvbuf")
		>>> {'g','i','j','r'}

		difference (Set [1..10]) (Set [0,(-1)..(-10)])
		>>> {1,2,3,4,5,6,7,8,9,10}

		difference (Set ["s","d","l","i","r","e","v"]) (Set ["a","e", "i", "o", "u"])
		>>> {"d","l","r","s","v"}
-}
difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) (Set set2)
	= Set ((L.sort . L.nub) [e | e <- set1, not (elem e set2)])


-- Membership
{-
	Usage:
		isMemberOf 2 natural
		>>> True

		isMemberOf 2 (Set [(-4),(-2)..10])
		>>> True

		'a' `isMemberOf` (Set "aeiou")
		>>> True
		
		isMemberOf 0 (Set (natural' 10))
		>>> False
-}
isMemberOf :: Eq a => Element a -> Set a -> Bool
isMemberOf a (Set []) = False
isMemberOf a (Set set) = a `elem` set


-- Cardinality
{-
	Usage:
		cardinality (Set ['a', 'a', 'b', 'c'])
		>>> 3
		
		cardinality (Set [1..10])
		>>> 10
		
		cardinality (Set [x | x <- (natural' 100), even x])
		>>> 50
		
		cardinality (Set "ksjbgvkbsdjkbfkujasfdvbkh")
		>>> 11
-}
cardinality :: Eq a => Set a -> Int
cardinality (Set set) = (L.length . L.nub) set


-- Empty/Null Set verification
{-
	Usage:
		isNullSet (Set [x | x <- [1,3..100], even x])
		>>> True

		isNullSet (Set [])
		>>> True

		isNullSet (Set [1..6])
		>>> False

		isNullSet $ intersection (Set [0,2..10]) (Set [1,3..10])
		>>> True
-}
isNullSet :: Eq a => Set a -> Bool
isNullSet (Set set)
	| cardinality (Set set) == 0 = True
	| otherwise = False


-- Subset verification
{-
	Usage:
		isSubset (Set [1,2]) (Set [1..50])
		>>> True

		isSubset (Set [1,2]) (Set [1,3..50])
		>>> False
		
		isSubset (Set [1..10]) (Set [1,2,3,4])
		>>> False
		
		isSubset (Set (natural' 10)) (Set (whole' 10))
		>>> True
-}
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset (Set set1) (Set set2) = null [e | e <- set1', not (elem e set2')]
	where	set1' = (L.sort . L.nub) set1
		set2' = (L.sort . L.nub) set2


-- Superset verification
{-
	Usage:
		isSuperset (Set [1..50]) (Set [1,2])
		>>> True

		isSuperset (Set [1,3..50]) (Set [2,4,6])
		>>> False
		
		isSuperset (Set [1..10]) (Set [1,2,3,4])
		>>> True

		isSuperset (Set (natural' 10)) (Set (whole' 10))
		>>> False
-}
isSuperset :: Ord a => Set a -> Set a -> Bool
isSuperset (Set set1) (Set set2) = null [e | e <- set2', not (elem e set1')]
	where	set1' = (L.sort . L.nub) set1
		set2' = (L.sort . L.nub) set2


-- Power set
{-
	Usage:
		powerSet (Set ['x','y','z'])
		>>> {{},{'z'},{'y'},{'y','z'},{'x'},{'x','z'},{'x','y'},{'x','y','z'}}

		powerSet (Set [1,2])
		>>> {{},{2},{1},{1,2}}
		
		powerSet (Set "12")
		>>> {{},{'2'},{'1'},{'1','2'}}

		cardinality $ powerSet (Set "12")
		>>> 4
-}
powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set s) = Set $ map (\xs -> (Set xs)) (L.subsequences $ set2list (Set s))


powerSet' :: Ord a => Set a -> Set (Set a)
powerSet' (Set xs) = Set $ L.sort (map (\xs -> (Set xs)) (powerList xs))

powerList :: Ord a => [a] -> [[a]]
--powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = L.sort $ (powerList xs) ++ (map (x:) (powerList xs))


-- Cartesian product
{-
	Usage:
		cartProduct (Set [1,2,3]) (Set [2,4,6])
		>>> [(1,2),(1,4),(1,6),(2,2),(2,4),(2,6),(3,2),(3,4),(3,6)]

		cartProduct (Set "abc") (Set "de")
		>>> [('a','d'),('a','e'),('b','d'),('b','e'),('c','d'),('c','e')]

		cartProduct (Set []) (Set "pq")
		>>> []

		cartProduct (Set "pq") (Set [])
		>>> []

		(cartProduct (Set [1,2]) (Set [3,4])) == (cartProduct (Set [3,4]) (Set [1,2]))
		>>> False
-}
cartProduct :: Ord a => Set a -> Set a -> Set (Element a,Element a)
cartProduct (Set set1) (Set set2) = Set $ [(x,y) | x <- set1', y <- set2']
	where	set1' = (L.sort . L.nub) set1
		set2' = (L.sort . L.nub) set2


-- Checking if two sets are disjoint
{-
	Usage:
		disjoint (Set [1..10]) (Set [1,2,3])
		>>> False

		disjoint (Set [2,4..100]) (Set [1,3..9])
		>>> True

		disjoint (Set (natural' 1000)) (Set (whole' 100))
		>>> False

		disjoint (Set "dshbchsdb") (Set "yruiretu")
		>>> True
-}
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint (Set set1) (Set set2) = isNullSet $ intersection (Set set1) (Set set2)


-- Checking if all Sets in a list are disjoint
{-
	Usage:
		s4
		>>> {1,2,3,4,5,6,7,8,9,10}

		s5
		>>> {0,2,4,6,8,10}

		s6
		>>> {0,4,8,12}

		disjointL [s4,s5,s6]
		>>> False

		disjointL [(Set [1,2]), (Set [3..10])]
		>>> True
-}
disjointL :: Ord a => [Set a] -> Bool
disjointL s = isNullSet $ intersectionL s


-- Set of natural numbers
natural = [1,2..]


-- Set of natural numbers upto n
natural' n = [1,2..n]


-- Set of whole numbers
whole = [0,1..]


-- Set of whole numbers upto n
whole' n = [0,1..n]


-- Mapping a function to a Set
{-
	Usage:
		sMap (*2) (Set [1..10])
		>>> {2,4,6,8,10,12,14,16,18,20}
		
		sMap (show) (Set [1..10])
		>>> {"1","10","2","3","4","5","6","7","8","9"}
-}
sMap f (Set s) = list2set $ map f s


-- SAMPLE SETS --
s1 = Set [1,3,5,7,9]

s2 = Set [2,4..100]

s3 = Set []
