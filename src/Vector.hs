{-|
Module      : Vector
Description : Vector module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
    * Vector addition / resultant
    * Vector subtraction
    * Vector multiplication
    * Inner/Scalar product (dot product)
    * Vector product (cross product)
    * Scalar triple product
    * Vector triple product
    * Vector equality
    * Null vector
    * Vector dimension / order
    * Vector magnitude
    * Angle between two vectors
    * Orthogonality of vectors
    * Mapping of functions to vectors
-}

module Vector
(
    Vector(..),
    dimension,
    magnitude,
    vectorToList,
    listToVector,
    vAdd,
    vAddL,
    (<+>),
    vSub,
    vSubL,
    (<->),
    innerProd,
    (<.>),
    angle,
    scalarMult,
    (<*>),
    isNullVector,
    crossProduct,
    (><),
    scalarTripleProduct,
    vectorTripleProduct,
    extract,
    extractRange,
    areOrthogonal,
    vMap,
    normalize
)
where

import Prelude hiding ((<*>))

import qualified Data.List as L


{-|
    The 'Vector' type is used to represent vectors. Internally, 'Vector' is represented as a list.

    For example:

    >>> v1
    <1,2>

    >>> v2
    <1,-1,1>
-}
newtype Vector a = Vector [a] deriving (Eq)

instance (Show a) => Show (Vector a) where
    showsPrec _ (Vector s) = showVector s

showVector [] str = showString "<>" str
showVector (x:xs) str = showChar '<' (shows x (showl xs str))
    where 
        showl [] str = showChar '>' str
        showl (x:xs) str = showChar ',' (shows x (showl xs str))


{-|
    The 'vectorToList' function converts a 'Vector' to a list.

    For example:

    >>> v4
    <1,2,3>

    >>> vectorToList v4
    [1,2,3]
-}
vectorToList :: Eq a => Vector a -> [a]
vectorToList (Vector []) = []
vectorToList (Vector v) = v


{-|
    The 'listToVector' function converts a list to a 'Vector'.

    For example:

    >>> listToVector [1,2,3]
    <1,2,3>
-}
listToVector :: [a] -> Vector a
listToVector v = Vector v


{-|
    The 'dimension' function returns the dimension or order of a 'Vector'.

    For example:

    >>> dimension (Vector [1,1,3])
    3
    
    >>> dimension (Vector [1..27])
    27
    
    >>> dimension (Vector [])
    0
-}
dimension :: Vector a -> Int
dimension (Vector vector) = length vector


{-|
    The 'magnitude' function returns the magnitude of a 'Vector'.

    For example:

    >>> magnitude (Vector [1,1])
    1.4142135623730951
    
    >>> magnitude (Vector [0,0,2.5])
    2.5
    
    >>> magnitude (Vector [])
    0.0
-}
magnitude :: (Num a, Floating a) => Vector a -> a
magnitude (Vector []) = 0
magnitude (Vector v) = sqrt (sum [x*x | x <- v])


{-|
    The 'vAdd' function performs addition of two vectors.

    For example:

    >>> vAdd (Vector [1,2,3]) (Vector [0,10])
    <1,12,3>
    
    >>> vAdd (Vector [1,2,3]) (Vector [0,(-10),(-3)])
    <1,-8,0>
    
    >>> vAdd (Vector [1,2,3]) (Vector [])
    <1,2,3>
-}
vAdd :: Num a => Vector a -> Vector a -> Vector a
vAdd (Vector []) (Vector []) = Vector []
vAdd (Vector []) (Vector v) = Vector v
vAdd (Vector v) (Vector []) = Vector v
vAdd (Vector v1) (Vector v2) = Vector $ vAdd' (Vector v1) (Vector v2)

-- internal function
vAdd' :: Num a => Vector a -> Vector a -> [a]
vAdd' (Vector (a:as)) (Vector (b:bs))
    | null bs = (a+b) : as
    | null as = (a+b) : bs
    | length as == length bs = (a+b) : vAdd' (Vector as) (Vector bs)
    | length as < length bs = (a+b) : vAdd' (Vector (as ++ [0])) (Vector bs)
    | length as > length bs = (a+b) : vAdd' (Vector as) (Vector (bs ++ [0]))


{-|
    The '<+>' operator can also be used to add two vectors.

    For example:

    >>> (Vector [1]) <+> (Vector [1,2,0,3])
    <2,2,0,3>
-}
(<+>) :: Num a => Vector a -> Vector a -> Vector a
(<+>) = vAdd


{-|
    The 'vAddL' function adds a list of vectors.

    For example:
    
    >>> v1
    <1,2>

    >>> v2
    <1,1,1>

    >>> v3
    <>

    >>> vAddL [v1, v2, v3]
    <2,3,1>
-}
vAddL :: Num a => [Vector a] -> Vector a
vAddL = foldl1 vAdd


{-|
    The 'vSub' function is used to subtract a 'Vector' fromm another.

    For example:

    >>> (Vector []) `vSub` (Vector [])
    <>
    
    >>> (Vector [1]) `vSub` (Vector [])
    <1.0>
    
    >>> vSub (Vector []) (Vector [1.5,6])
    <-1.5,-6.0>
-}
vSub (Vector []) (Vector []) = Vector []
vSub (Vector v) (Vector []) = Vector v
vSub (Vector v1) (Vector v2) = vAdd (Vector v1) (scalarMult (-1) (Vector v2))


{-|
    The '<->' operator can also be used for 'Vector' subtraction.

    For example:

    >>> (Vector [7,8,0,5.2]) <-> (Vector [1,2,3])
    <6.0,6.0,-3.0,5.2>
-}
(<->) :: Num a => Vector a -> Vector a -> Vector a
(<->) = vSub


{-|
    The 'vSubL' function subtracts a list of vectors.

    For example:
        
    >>> v1
    <1,2>

    >>> v2
    <1,1,1>

    >>> v3
    <>
    
    >>> vSubL [v1,v2,v3]
    <0,1,-1>
-}
vSubL :: Num a => [Vector a] -> Vector a
vSubL = foldl1 vSub


{-|
    The 'innerProd' function returns the inner product (scalar/dot product) of two vectors.

    For example:

    >>> innerProd (Vector [1,2,3]) (Vector [1.1])
    1.1
    
    >>> innerProd (Vector [(-1)]) (Vector [(-1)])
    1
    
    >>> innerProd (Vector []) (Vector [])
    0
    
    >>> innerProd (Vector [1,1,1]) (Vector [2.5,2.5,2.5])
    7.5
-}
innerProd :: Num a => Vector a -> Vector a -> a
innerProd (Vector []) (Vector []) = 0
innerProd (Vector []) (Vector a) = 0
innerProd (Vector a) (Vector []) = 0
innerProd (Vector (a:as)) (Vector (b:bs))
    | null as || null bs = (a * b) + innerProd (Vector as) (Vector bs)
    | length as == length bs = (a * b) + innerProd (Vector as) (Vector bs)
    | length as < length bs = (a * b) + innerProd (Vector (as ++ [0])) (Vector bs)
    | length as > length bs = (a * b) + innerProd (Vector as) (Vector (bs ++ [0]))


{-|
    The '<.>' operator can also be used to calculate the inner product.

    For example:

    >>> (Vector [1,1,1]) <.> (Vector [1..10])
    6
-}
(<.>) :: Num a => Vector a -> Vector a -> a
(<.>) = innerProd


{-|
    The 'angle' function returns the angle (in degree) between two vectors.

    For example:

    >>> angle (Vector [1,1,1]) (Vector [1..10])
    79.83130545524068

    >>> angle (Vector [1,1,1]) (Vector [2,2,2])
    NaN

    >>> angle (Vector [1,1,1]) (Vector [0,0,0])
    NaN

    >>> angle (Vector [1,1,1]) (Vector [2.5,2.5])
    35.264389682754654

    >>> angle (Vector [1..4]) (Vector [1..4])
    0.0
-}
angle :: Floating a => Vector a -> Vector a -> a
angle (Vector []) (Vector []) = 0
angle (Vector v1) (Vector v2) = (180/pi) * acos (innerProd (Vector v1) (Vector v2) / ( magnitude (Vector v1) * magnitude (Vector v2) ))


{-|
    The 'scalarMult' function multiplies a scalar value to a 'Vector'.

    For example:

    >>> scalarMult 3 (Vector [1,1,1])
    <3.0,3.0,3.0>

    >>> scalarMult (2/3) (Vector [1.5,1.5,1.5])
    <1.0,1.0,1.0>

    >>> scalarMult (-1) (Vector [1.5,1.5,1.5])
    <-1.5,-1.5,-1.5>

    >>> scalarMult (0) (Vector [1,2,32])
    <0.0,0.0,0.0>
-}
scalarMult :: Num a => a -> Vector a -> Vector a
scalarMult scalar (Vector vector) = Vector $ map (* scalar) vector


{-|
    The '<*>' operator can also be used for scalar multiplication.

    For example:

    >>> 3 <*> (Vector [1,1,1])
    <3,3,3>
-}
(<*>) :: Num a => a -> Vector a -> Vector a
(<*>) = scalarMult


{-|
    The 'isNullVector' function checks if a 'Vector' is null or not.
    
    For example:

    >>> isNullVector (Vector [])
    True

    >>> isNullVector (Vector [0,0,0])
    True

    >>> isNullVector (Vector [1,2,3])
    False

    >>> isNullVector $ scalarMult 0 (Vector [1,5])
    True
-}
isNullVector :: (Eq a, Floating a) => Vector a -> Bool
isNullVector (Vector v) = magnitude (Vector v) == 0


{-|
    The 'crossProduct' function returns the cross-product of two vectors.

    For example:

    >>> crossProduct (Vector [1,1,1]) (Vector [1,1,1])
    <0,0,0>

    >>> crossProduct (Vector [1,1,1]) (Vector [])
    <0,0,0>

    >>> crossProduct (Vector [1,1,1]) (Vector [3,4,5,6])
    *** Exception: Order of vectors must not exceed 3.
-}
crossProduct :: Num a => Vector a -> Vector a -> Vector a
crossProduct (Vector a) (Vector b)
    | length a < 3 = crossProduct (Vector (a ++ [0])) (Vector b)
    | length b < 3 = crossProduct (Vector a) (Vector (b ++ [0]))
    | (length a == 3) && (length b == 3) = Vector [ (a!!1 * b!!2) - (a!!2 * b!!1), (a!!2 * head b) - (head a * b!!2), (head a * b!!1) - (a!!1 * head b) ]
    | (length a > 3) || (length b > 3) = error "Order of vectors must not exceed 3.\n"


{-|
    The '><' operator can also be used to calculate the cross-product.

    For example:
    
    >>> (Vector [0,0]) >< (Vector [0,0,0])
    <0,0,0>
-}
(><) :: Num a => Vector a -> Vector a -> Vector a
(><) = crossProduct


{-|
    The 'scalarTripleProduct' function returns the Scalar Triple Product of three vectors.
    
    For example:

    >>> scalarTripleProduct (Vector [1,1,1]) (Vector [2,3,4]) (Vector [1,2,0])
    -3

    >>> scalarTripleProduct (Vector [0]) (Vector [0,0]) (Vector [0,0,0])
    0

    >>> scalarTripleProduct (Vector []) (Vector []) (Vector [])
    0

    >>> scalarTripleProduct (Vector [2,7.9]) (Vector [3]) (Vector [(-1)])
    0.0
-}
scalarTripleProduct :: Num a => Vector a -> Vector a -> Vector a -> a
scalarTripleProduct a b c = innerProd a (crossProduct b c)


{-|
    The 'vectorTripleProduct' function returns the Vector Triple Product of three vectors.

    For example:
    
    >>> vectorTripleProduct (Vector [1,1]) (Vector [2,3]) (Vector [4,5,6])
    <-2,2,-30>

    >>> vectorTripleProduct (Vector [0,0,0]) (Vector [1.3,2.8]) (Vector [4,5.5])
    <-0.0,0.0,0.0>

    >>> vectorTripleProduct (Vector []) (Vector []) (Vector [])
    <0,0,0>
-}
vectorTripleProduct :: Num a => Vector a -> Vector a -> Vector a -> Vector a
vectorTripleProduct a b c = crossProduct a (crossProduct b c)


{-|
    The 'extract' function is used to extract a particular element of a vector.

    For example:

    >>> extract 2 (Vector [1..4])
    3

    >>> extract 8 (Vector [1,1.1..4])
    1.8000000000000007

    >>> extract 0 (Vector [1..27])
    1

    >>> extract 3 (Vector [1,2])
    *** Exception: The index mentioned is larger than the dimension of the Vector.
-}
extract :: Int -> Vector a -> a
extract n (Vector vector)
    | n <= dimension (Vector vector) = vector !! n
    | otherwise = error "The index mentioned is larger than the dimension of the Vector."


{-|
    The 'extractRange' function is used to extract a range of elements from a vector.

    For example:

    >>> extractRange 2 6 (Vector [1..10])
    <3,4,5,6,7>

    >>> extractRange 0 0 (Vector [1..10])
    <1>
-}
extractRange :: Eq a => Int -> Int -> Vector a -> Vector a
extractRange a b (Vector vector) = Vector [ extract i (Vector vector) | i <- [a..b] ]


{-|
    The 'areOrthogonal' function checks if two vectors are orthogonal.

    For extract:

    >>> areOrthogonal (Vector [1,1]) (Vector [0,1,1])
    False

    >>> areOrthogonal (Vector [1]) (Vector [0])
    True

    >>> areOrthogonal (Vector [1,0,0]) (Vector [0,1.576,0])
    True
-}
areOrthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
areOrthogonal (Vector v1) (Vector v2) = innerProd (Vector v1) (Vector v2) == 0


{-|
    The 'vMap' function maps a function to a 'Vector' passed as argument.

    For example:

    >>> vMap (* 2) (Vector [1,2,3])
    <2,4,6>
    
    >>> vMap (/2) (Vector [1,2,7])
    <0.5,1.0,3.5>
-}
vMap f (Vector v) = Vector $ map f v


{-|
    The 'vNorm' function normalizes a 'Vector' passed as argument.

    For example:

    >>> vNorm (Vector [1,2,3])
    <0.2672612419124244,0.5345224838248488,0.8017837257372732>

    >>> vNorm (Vector [1..5])
    <0.13483997249264842,0.26967994498529685,0.40451991747794525,0.5393598899705937,0.6741998624632421>
-}
normalize :: (Num a, Floating a, Fractional a) => Vector a -> Vector a
normalize (Vector v) = scalarMult (1 / magnitude (Vector v)) (Vector v)


-- SAMPLE VECTORS --
v1 = Vector [1,2]

v2 = Vector [1,-1,1]

v3 = Vector []
