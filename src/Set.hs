{-|
Module      : Set
Description : Set module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
    * Union
    * Intersection
    * Difference
    * Membership
    * Cardinality
    * Null set verification
    * Subset verification
    * Superset verification
    * Power set generation
    * Cartesian product
    * Set equality
    * Set disjoint verification
-}

module Set 
(
    Set(..),
    setToList,
    listToSet,
    union,
    unionL,
    intersection,
    intersectionL,
    difference,
    isMemberOf,
    isNotMemberOf,
    cardinality,
    isNullSet,
    isNotNullSet,
    isSubset,
    isProperSubset,
    isSuperset,
    powerSet,
    cartProduct,
    areDisjoint,
    areDisjointL,
    nullSet,
    natural,
    natural',
    whole,
    whole',
    sMap
)
where


import qualified Data.List as L


{-|
    The 'Set' type is used for representing sets.

    For example:

    >>> Set [1..5]
    {1,2,3,4,5}

    >>> Set [ even x | x <- [1,2,3,4] ]
    {False,True,False,True}
-}
newtype Set a = Set [a] deriving (Ord)

instance Eq a => Eq (Set a) where
    set1 == set2 = isSubset set1 set2 && isSubset set2 set1

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) = showSet s


showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' (shows x (showl xs str))
    where 
        showl [] str = showChar '}' str
        showl (x:xs) str = showChar ',' (shows x (showl xs str))


{-|
    The 'isSubset' function takes two 'Set's as arguments and checks if the first 'Set' is a subset of the second 'Set'.
    
    For example:

    >>> isSubset (Set [1,2]) (Set [1..50])
    True

    >>> isSubset (Set [1,2]) (Set [1,3..50])
    False
    
    >>> isSubset (Set [1..10]) (Set [1,2,3,4])
    False
    
    >>> isSubset (Set (natural' 10)) (Set (whole' 10))
    True
-}
isSubset :: (Eq a) => Set a -> Set a -> Bool
isSubset (Set []) _ = True
isSubset (Set (x:xs)) set = isMemberOf x set && isSubset (Set xs) set


{-|
    The 'isProperSubset' function takes two 'Set's as arguments and checks if the first 'Set' is a proper subset of the second 'Set'.
    
    For example:

    >>> s1
    {1,3,5,7,9}

    >>> s4
    {1,3,5}

    >>> isProperSubset s4 s1
    True

    >>> s5
    {1,3,5,7,9}

    >>> isProperSubset s5 s1
    False
-}
isProperSubset :: (Eq a) => Set a -> Set a -> Bool
isProperSubset (Set set1) (Set set2) = (set1 /= set2) && (isSubset (Set set1) (Set set2))


{-|
    The 'listToSet' function converts a list to 'Set'.

    For example:

    >>> let l = [1,2,3]
    >>> let s = listToSet l
    >>> s
    Set {1,2,3}
-}
listToSet :: Ord a => [a] -> Set a
listToSet [] = Set []
listToSet list = Set ((L.sort . L.nub) list)


{-|
    The 'setToList' function converts a 'Set' into a list and returns the list.

    For example:

    >>> s1
    {1,2,3,4,5,6,7,8,9,10}
    
    >>> setToList s1
    [1,2,3,4,5,6,7,8,9,10]
-}
setToList :: Set a -> [a]
setToList (Set s) = s


{-|
    The 'union' function performs the set union operation on two sets that are passed as arguments.

    For example:

    >>> union (Set [2,4,6]) (Set [1,2,3])
    {1,2,3,4,6}
    
    >>> union (Set ["a", "b"]) (Set ["b", "c", "d"])
    {"a","b","c","d"}
    
    >>> union (Set [2,4..10]) (Set [1,3..10])
    {1,2,3,4,5,6,7,8,9,10}
    
    >>> union (Set "hdbshv") (Set "dsjnru")
    {'b','d','h','j','n','r','s','u','v'}
-}
union :: Ord a => Set a -> Set a -> Set a
union (Set set1) (Set set2)
    = Set ((L.sort . L.nub) (set1 ++ [e | e <- set2, e `notElem` set1]))


{-|
    The 'unionL' function performs the set union operation on a list of sets that are passed as argument.

    For example:
    
    >>> s1
    {1,3,5,7,9}
    
    >>> s2
    {2,4,6,8,10}

    >>> s3
    {}

    >>> unionL [s1,s2,s3]
    {1,2,3,4,5,6,7,8,9,10}
-}
unionL :: Ord a => [Set a] -> Set a
unionL = foldl1 union


{-|
    The 'intersection' function performs the set intersection operation on two sets that are passed as arguments.

    For example:
        
    >>> intersection (Set [2,4,6]) (Set [1,2,3])
    {2}

    >>> intersection (Set ["a", "b"]) (Set ["b", "c", "d"])
    {"b"}

    >>> intersection (Set [2,4..10]) (Set [1,3..10])
    {}

    >>> intersection (Set "hdbshv") (Set "dsjnru")
    {'d','s'}
-}
intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set set1) (Set set2)
    = Set ((L.sort . L.nub) [e | e <- set1, e `elem` set2])


{-|
    The 'intersectionL' function performs the set intersection operation on a list of sets.

    For example:
        
    >>> s4
    {1,2,3,4,5,6,7,8,9,10}

    >>> s5
    {0,2,4,6,8,10}

    >>> s6
    {2,4,6,18}

    >>> intersectionL [s4,s5,s6]
    {2,4,6}
-}
intersectionL :: Ord a => [Set a] -> Set a
intersectionL = foldl1 intersection


{-|
    The 'difference' function performs the set difference operation on the two sets passed as arguments.

    For example:

    >>> difference (Set [1,2,3]) (Set [2..5])
    {1}

    >>> difference (Set "irjfg") (Set "kdsvbuf")
    {'g','i','j','r'}

    >>> difference (Set [1..10]) (Set [0,(-1)..(-10)])
    {1,2,3,4,5,6,7,8,9,10}

    >>> difference (Set ["s","d","l","i","r","e","v"]) (Set ["a","e", "i", "o", "u"])
    {"d","l","r","s","v"}
-}
difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) (Set set2)
    = Set ((L.sort . L.nub) [e | e <- set1, e `notElem` set2])


{-|
    The 'isMemberOf' function checks if an element is a member of a 'Set'.

    For example:
    
    >>> isMemberOf 2 natural
    True

    >>> isMemberOf 2 (Set [(-4),(-2)..10])
    True

    >>> 'a' `isMemberOf` (Set "aeiou")
    True
    
    >>> isMemberOf 0 (Set (natural' 10))
    False
-}
isMemberOf :: Eq a => a -> Set a -> Bool
isMemberOf a (Set []) = False
isMemberOf a (Set set) = a `elem` set


{-|
    The 'isNotMemberOf' function checks if an element is not a member of a 'Set'.

    For example:

    >>> s1
    {1,3,5,7,9}

    >>> isNotMemberOf 2 s1
    True

    >>> isNotMemberOf 3 s1
    False
-}
isNotMemberOf :: Eq a => a -> Set a -> Bool
isNotMemberOf a (Set []) = True
isNotMemberOf a (Set set) = a `notElem` set


{-|
    The 'cardinality' function returns the number of elements in a 'Set'.

    For example:
        
    >>> cardinality (Set ['a', 'a', 'b', 'c'])
    3
    
    >>> cardinality (Set [1..10])
    10
    
    >>> cardinality (Set [x | x <- (natural' 100), even x])
    50
    
    >>> cardinality (Set "ksjbgvkbsdjkbfkujasfdvbkh")
    11
-}
cardinality :: Eq a => Set a -> Int
cardinality (Set set) = (L.length . L.nub) set


{-|
    The 'isNullSet' function checks if a 'Set' is null/empty.

    For example:

    >>> isNullSet (Set [x | x <- [1,3..100], even x])
    True

    >>> isNullSet (Set [])
    True

    >>> isNullSet (Set [1..6])
    False

    >>> isNullSet $ intersection (Set [0,2..10]) (Set [1,3..10])
    True
-}
isNullSet :: Eq a => Set a -> Bool
isNullSet (Set set) = cardinality (Set set) == 0


{-|
    The 'isNotNullSet' function checks if a 'Set' is not null/empty.

    For example:

    >>> isNotNullSet (Set [1,2,3])
    True

    >>> isNotNullSet (Set [])
    False

    >>> isNotNullSet (Set [Set[1,2]])
    True

    >>> isNotNullSet (Set [Set[]])
    True
-}
isNotNullSet :: Eq a => Set a -> Bool
isNotNullSet (Set set) = cardinality (Set set) /= 0


{-|
    The 'isSuperset' function checks if a 'Set' is a superset of another 'Set', i.e. all elements of the second 'Set' are members of the first 'Set'.

    For example:
        
    >>> isSuperset (Set [1..50]) (Set [1,2])
    True

    >>> isSuperset (Set [1,3..50]) (Set [2,4,6])
    False
    
    >>> isSuperset (Set [1..10]) (Set [1,2,3,4])
    True

    >>> isSuperset (Set (natural' 10)) (Set (whole' 10))
    False
-}
isSuperset :: Ord a => Set a -> Set a -> Bool
isSuperset (Set set1) (Set set2) = null [e | e <- set2', e `notElem` set1']
    where
        set1' = (L.sort . L.nub) set1
        set2' = (L.sort . L.nub) set2


{-|
    The 'powerSet' function returns the power set of a 'Set' passed as argument.

    For example:

    >>> powerSet (Set ['x','y','z'])
    {{},{'z'},{'y'},{'y','z'},{'x'},{'x','z'},{'x','y'},{'x','y','z'}}

    >>> powerSet (Set [1,2])
    {{},{2},{1},{1,2}}
    
    >>> powerSet (Set "12")
    {{},{'2'},{'1'},{'1','2'}}

    >>> cardinality $ powerSet (Set "12")
    4
-}
powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set s) = Set $ map Set (L.subsequences $ setToList (Set s))


{-
powerSet' :: Ord a => Set a -> Set (Set a)
powerSet' (Set xs) = Set $ L.sort (map Set (powerList xs))

powerList :: Ord a => [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = L.sort $ powerList xs ++ map (x:) (powerList xs)
-}


{-|
    The 'cartProduct' function returns the Cartesian Product of two Sets passed as arguments.

    For example:

    >>> cartProduct (Set [1,2,3]) (Set [2,4,6])
    [(1,2),(1,4),(1,6),(2,2),(2,4),(2,6),(3,2),(3,4),(3,6)]

    >>> cartProduct (Set "abc") (Set "de")
    [('a','d'),('a','e'),('b','d'),('b','e'),('c','d'),('c','e')]

    >>> cartProduct (Set []) (Set "pq")
    []

    >>> cartProduct (Set "pq") (Set [])
    []

    >>> (cartProduct (Set [1,2]) (Set [3,4])) == (cartProduct (Set [3,4]) (Set [1,2]))
    False
-}
cartProduct :: Ord a => Set a -> Set a -> Set (a, a)
cartProduct (Set set1) (Set set2) = Set [(x,y) | x <- set1', y <- set2']
    where
        set1' = (L.sort . L.nub) set1
        set2' = (L.sort . L.nub) set2


{-|
    The 'areDisjoint' function checks if two Sets are disjoint, i.e. have no common elements.

    For example:

    >>> areDisjoint (Set [1..10]) (Set [1,2,3])
    False

    >>> areDisjoint (Set [2,4..100]) (Set [1,3..9])
    True

    >>> areDisjoint (Set (natural' 1000)) (Set (whole' 100))
    False

    >>> areDisjoint (Set "dshbchsdb") (Set "yruiretu")
    True
-}
areDisjoint :: Ord a => Set a -> Set a -> Bool
areDisjoint (Set set1) (Set set2) = isNullSet $ intersection (Set set1) (Set set2)


{-|
    The 'areDisjointL' function checks if all Sets in a list of Sets are disjoint.

    For example:

    >>> s4
    {1,2,3,4,5,6,7,8,9,10}

    >>> s5
    {0,2,4,6,8,10}

    >>> s6
    {0,4,8,12}

    >>> areDisjointL [s4,s5,s6]
    False

    >>> areDisjointL [(Set [1,2]), (Set [3..10])]
    True
-}
areDisjointL :: Ord a => [Set a] -> Bool
areDisjointL s = isNullSet $ intersectionL s


{-|
    The 'nullSet' function returns a null 'Set'.
-}
nullSet = Set []


{-|
    The 'natural' function returns the 'Set' of all natural numbers (starting at 1.
-}
natural = Set [1,2..]


{-|
    The 'natural'' function returns natural numbers upto a specified limit. 
-}
natural' n = Set [1,2..n]


{-|
    The 'whole' function returns the 'Set' of all whole numbers (starting at 0).
-}
whole = Set [0,1..]


{-|
    The 'whole'' function returns the 'Set' of all whole numbers upto a specified limit.
-}
whole' n = Set [0,1..n]


{-|
    The 'sMap' function maps a function to a 'Set'.

    The function takes two arguments, the first if the function and the second is the 'Set' on which the function is to be applied.

    For example:
    
    >>> sMap (*2) (Set [1..10])
    {2,4,6,8,10,12,14,16,18,20}
    
    >>> sMap (show) (Set [1..10])
    {"1","10","2","3","4","5","6","7","8","9"}
-}
sMap f (Set s) = listToSet $ map f s


-- SAMPLE SETS --
s1 = Set [1,3,5,7,9]

s2 = Set [2,4..10]

s3 = Set []
