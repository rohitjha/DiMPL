{-
--------------------------
| Vectors Module for MPL |
--------------------------

Functionality for:
	-> Vectors
		-> Vector addition / resultant
		-> Vector subtraction
		-> Vector multiplication
		-> Inner/Scalar product (dot product)
		-> Vector product (cross product)
		-> Scalar triple product
		-> Vector triple product
		-> Vector equality
		-> Null vector
		-> Vector dimension / order
		-> Vector magnitude
		-> Angle between two vectors
		-> Orthogonality of vectors
		-> Mapping of functions to vectors

Author: Rohit Jha
Version: 0.1
Date: 7 Feb 2013
-}

module Vector
(
	Vector(..),
	vDim,
	vMag,
	vec2list,
	vAdd,
	vAddL,
	(<+>),
	vSub,
	vSubL,
	(<->),
	innerProd,
	(<.>),
	vAngle,
	scalarMult,
	(<*>),
	isNullVector,
	crossProd,
	(><),
	scalarTripleProd,
	vectorTripleProd,
	extract,
	extractRange,
	areOrthogonal,
	vMap,
	vNorm
)
where

import Prelude hiding ((<*>))

import qualified Data.List as L


newtype Vector a = Vector [a] deriving (Eq)

instance (Show a) => Show (Vector a) where
	showsPrec _ (Vector s) str = showVector s str

showVector [] str = showString "<>" str
showVector (x:xs) str = showChar '<' (shows x (showl xs str))
	where 
		showl [] str = showChar '>' str
		showl (x:xs) str = showChar ',' (shows x (showl xs str))


type Scalar a = a


-- Vector dimension / order
{-
	Usage:
		vDim (Vector [1,1,3])
		>>> 3
		
		vDim (Vector [1..27])
		>>> 27
		
		vDim (Vector [])
		>>> 0
-}
vDim :: Vector a -> Int
vDim (Vector vector) = length vector


-- Vector magnitude
{-
	Usage:
		vMag (Vector [1,1])
		>>> 1.4142135623730951
		
		vMag (Vector [0,0,2.5])
		>>> 2.5
		
		vMag (Vector [])
		>>> 0.0
-}
vMag :: Floating a => Vector a -> a
vMag (Vector []) = 0
vMag (Vector v) = (sum [x*x | x <- v]) ** 0.5


-- Vector addition
{-
	Usage:
		vAdd (Vector [1,2,3]) (Vector [0,10])
		>>> <1,12,3>
		
		vAdd (Vector [1,2,3]) (Vector [0,(-10),(-3)])
		>>> <1,-8,0>
		
		vAdd (Vector [1,2,3]) (Vector [])
		>>> <1,2,3>
		
		(Vector [0]) <+> (Vector [0,0,0,0])
		>>> <0,0,0,0>
-}
vAdd' :: Num a => Vector a -> Vector a -> [a]
vAdd' (Vector (a:as)) (Vector (b:bs))
	| (length bs == 0) = (a+b) : as
	| (length as == 0) = (a+b) : bs
	| (length as == length bs) = (a+b) : vAdd' (Vector as) (Vector bs)
	| (length as < length bs) = (a+b) : vAdd' (Vector (as ++ [0])) (Vector bs)
	| (length as > length bs) = (a+b) : vAdd' (Vector as) (Vector (bs ++ [0]))


vec2list :: Eq a => Vector a -> [a]
vec2list (Vector []) = []
vec2list (Vector v) = [x | x <- v]


vAdd :: Num a => Vector a -> Vector a -> Vector a
vAdd (Vector []) (Vector []) = (Vector [])
vAdd (Vector []) (Vector v) = Vector v
vAdd (Vector v) (Vector []) = Vector v
vAdd (Vector v1) (Vector v2) = Vector $ vAdd' (Vector v1) (Vector v2)


(<+>) = vAdd


-- Adding a list of Vectors
{-
	Usage:
		v1
		>>> <1,2>

		v2
		>>> <1,1,1>

		v3
		>>> <>

		vAddL [v1, v2, v3]
		>>> <2,3,1>
-}
vAddL :: Num a => [Vector a] -> Vector a
vAddL v = foldl1 (vAdd) v


-- Vector subtraction
{-
	Usage:
		(Vector []) `vSub` (Vector [])
		>>> <>
		
		(Vector [1]) `vSub` (Vector [])
		>>> <1.0>
		
		vSub (Vector []) (Vector [1.5,6])
		>>> <-1.5,-6.0>

		(Vector [7,8,0,5.2]) <-> (Vector [1,2,3])
		>>> <6.0,6.0,-3.0,5.2>
-}
vSub (Vector []) (Vector []) = Vector []
vSub (Vector v) (Vector []) = Vector v
vSub (Vector v1) (Vector v2) = vAdd (Vector v1) (scalarMult (-1) (Vector v2))


(<->) = vSub


-- Subtracting a list of vectors
{-
	Usage:
		v1
		>>> <1,2>

		v2
		>>> <1,1,1>

		v3
		>>> <>
		
		vSubL [v1,v2,v3]
		>>> <0,1,-1>
-}
vSubL :: Num a => [Vector a] -> Vector a
vSubL v = foldl1 (vSub) v


-- Inner product / Dot product / Scalar product
{-
	Usage:
		innerProd (Vector [1,2,3]) (Vector [1.1])
		>>> 1.1
		
		innerProd (Vector [(-1)]) (Vector [(-1)])
		>>> 1
		
		innerProd (Vector []) (Vector [])
		>>> 0
		
		innerProd (Vector [1,1,1]) (Vector [2.5,2.5,2.5])
		>>> 7.5

		(Vector [1,1,1]) <.> (Vector [1..10])
		>>> 6
-}
innerProd :: Num a => Vector a -> Vector a -> a
innerProd (Vector []) (Vector []) = 0
innerProd (Vector []) (Vector a) = 0
innerProd (Vector a) (Vector []) = 0
innerProd (Vector (a:as)) (Vector (b:bs))
	| (length as == 0 || length bs == 0) = (a * b) + innerProd (Vector as) (Vector bs)
	| (length as == length bs) = (a * b) + innerProd (Vector as) (Vector bs)
	| (length as < length bs) = (a * b) + innerProd (Vector (as ++ [0])) (Vector bs)
	| (length as > length bs) = (a * b) + innerProd (Vector as) (Vector (bs ++ [0]))


(<.>) = innerProd


-- Angle between two vectors
{-
	Usage:
		vAngle (Vector [1,1,1]) (Vector [1..10])
		>>> 1.3933191263592606

		vAngle (Vector [1,1,1]) (Vector [2,2,2])
		>>> NaN

		vAngle (Vector [1,1,1]) (Vector [0,0,0])
		>>> NaN

		vAngle (Vector [1,1,1]) (Vector [2.5,2.5])
		>>> 0.6154797086703874

		vAngle (Vector [1..4]) (Vector [1..4])
		>>> 0.0
-}
vAngle :: Floating a => Vector a -> Vector a -> a
vAngle (Vector []) (Vector []) = 0
vAngle (Vector v1) (Vector v2) = acos ((innerProd (Vector v1) (Vector v2)) / ( (vMag (Vector v1)) * (vMag (Vector v2)) ))


-- Multiplication by a scalar
{-
	Usage:
		scalarMult 3 (Vector [1,1,1])
		>>> <3.0,3.0,3.0>

		scalarMult (2/3) (Vector [1.5,1.5,1.5])
		>>> <1.0,1.0,1.0>

		scalarMult (-1) (Vector [1.5,1.5,1.5])
		>>> <-1.5,-1.5,-1.5>

		scalarMult (0) (Vector [1,2,32])
		>>> <0.0,0.0,0.0>
-}
scalarMult :: Num a => Scalar a -> Vector a -> Vector a
scalarMult scalar (Vector vector) = Vector $ map (* scalar) vector


(<*>) = scalarMult

-- Null vector checking
{-
	Usage:
		isNullVector (Vector [])
		>>> True

		isNullVector (Vector [0,0,0])
		>>> True

		isNullVector (Vector [1,2,3])
		>>> False

		isNullVector $ scalarMult 0 (Vector [1,5])
		>>> True
-}
isNullVector :: (Eq a, Floating a) => Vector a -> Bool
isNullVector (Vector v)
	| vMag (Vector v) == 0 = True
	| otherwise = False


-- Cross Product
{-
	Usage:
		crossProd (Vector [1,1,1]) (Vector [1,1,1])
		>>> <0,0,0>

		crossProd (Vector [1,1,1]) (Vector [])
		>>> <0,0,0>

		crossProd (Vector [1,1,1]) (Vector [3,4,5,6])
		>>> *** Exception: Order of vectors must not exceed 3.

		(Vector [0,0]) >< (Vector [0,0,0])
		>>> <0,0,0>
-}
crossProd :: Num a => Vector a -> Vector a -> Vector a
crossProd (Vector a) (Vector b)
	| (length a < 3) = crossProd (Vector (a ++ [0])) (Vector b)
	| (length b < 3) = crossProd (Vector a) (Vector (b ++ [0]))
	| (length a == 3) && (length b == 3) = Vector [ ( (a!!1 * b!!2) - (a!!2 * b!!1) ), ( (a!!2 * b!!0) - (a!!0 * b!!2) ), ( (a!!0 * b!!1) - (a!!1 * b!!0) ) ]
	| (length a > 3) || (length b > 3) = error "Order of vectors must not exceed 3.\n"

(><) = crossProd


-- Scalar triple product
{-
	Usage:
		scalarTripleProd (Vector [1,1,1]) (Vector [2,3,4]) (Vector [1,2,0])
		>>> -3

		scalarTripleProd (Vector [0]) (Vector [0,0]) (Vector [0,0,0])
		>>> 0

		scalarTripleProd (Vector []) (Vector []) (Vector [])
		>>> 0

		scalarTripleProd (Vector [2,7.9]) (Vector [3]) (Vector [(-1)])
		>>> 0.0
-}
--scalarTripleProd :: Num a => [a] -> [a] -> [a] -> Scalar a
scalarTripleProd a b c = innerProd a (crossProd b c)


-- Vector triple product
{-
	Usage:
		vectorTripleProd (Vector [1,1]) (Vector [2,3]) (Vector [4,5,6])
		>>> <-2,2,-30>

		vectorTripleProd (Vector [0,0,0]) (Vector [1.3,2.8]) (Vector [4,5.5])
		>>> <-0.0,0.0,0.0>

		vectorTripleProd (Vector []) (Vector []) (Vector [])
		>>> <0,0,0>
-}
vectorTripleProd :: Num a => Vector a -> Vector a -> Vector a -> Vector a
vectorTripleProd a b c = crossProd a (crossProd b c)


-- Extract particular element of a vector
{-
	Usage:
		extract 2 (Vector [1..4])
		>>> 3

		extract 8 (Vector [1,1.1..4])
		>>> 1.8000000000000007

		extract 0 (Vector [1..27])
		>>> 1
-}
extract :: Int -> Vector a ->Scalar a
extract n (Vector vector) = vector !! n


-- Extract a range of elements from a vector
{-
	Usage:
		extractRange 2 6 (Vector [1..10])
		>>> <3,4,5,6,7>

		extractRange 0 0 (Vector [1..10])
		>>> <1>
-}
extractRange :: Eq a => Int -> Int -> Vector a -> Vector a
extractRange a b (Vector vector) = Vector [ extract i (Vector vector) | i <- [a..b] ]


-- Checking if two vectors are orthogonal
{-
	Usage:
		areOrthogonal (Vector [1,1]) (Vector [0,1,1])
		>>> False

		areOrthogonal (Vector [1]) (Vector [0])
		>>> True

		areOrthogonal (Vector [1,0,0]) (Vector [0,1.576,0])
		>>> True
-}
areOrthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
areOrthogonal (Vector v1) (Vector v2) = (innerProd (Vector v1) (Vector v2)) == 0


-- Mapping a function to a vector
{-
	Usage:
		vMap (* 2) (Vector [1,2,3])
		>>> <2,4,6>
		
		vMap (/2) (Vector [1,2,7])
		>>> [0.5,1.0,3.5]
-}
vMap f (Vector v) = Vector $ map f v


-- Vector normalization
{-
	Usage:
		vNorm (Vector [1,2,3])
		>>> <0.2672612419124244,0.5345224838248488,0.8017837257372732>

		vNorm (Vector [1..5])
		>>> <0.13483997249264842,0.26967994498529685,0.40451991747794525,0.5393598899705937,0.6741998624632421>
-}
vNorm (Vector v) = scalarMult (1/(vMag (Vector v))) (Vector v)


-- SAMPLE VECTORS --
v1 = Vector [1,2]

v2 = Vector [1,(-1),1]

v3 = Vector []
