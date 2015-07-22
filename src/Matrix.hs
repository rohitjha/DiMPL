{-
-------------------------
| Matrix Module for MPL |
-------------------------

Functionality for:
    -> Matrix
        -> Matrix addition
        -> Matrix subtraction
        -> Matrix multiplication
        -> Multiplication with a scalar
        -> Matrix division
        -> Matrix transposition
        -> Matrix inversion
        -> Finding number of rows and columns
        -> Matrix equality
        -> Extractig row/s and column/s
        -> Power of matrices
        -> Trace of a matrix
        -> Checking for type of matrix
        (symmetric, skew-symmetric, row, column, square, zero, one, unit/identity, orthogonal, involutive, invertible)
    -> Determinant
        -> Calculating determinant of a matrix
        -> Finding minors and cofactors

Author: Rohit Jha
Version: 0.1
Date: 31 Jan 2013
-}

module Matrix
(
    Matrix(..),
    mAdd,
    mAddL,
    (|+|),
    mSub,
    (|-|),
    mTranspose,
    mScalarMult,
    (|*|),
    mMult,
    mMultL,
    (|><|),
    numRows,
    numCols,
    mat2list,
    determinant,
    inverse,
    mDiv,
    (|/|),
    extractRow,
    extractCol,
    extractRowRange,
    extractColRange,
    mPower,
    trace,
    isInvertible,
    isSymmetric,
    isSkewSymmetric,
    isRow,
    isColumn,
    isSquare,
    isOrthogonal,
    isInvolutive,
    isZeroOne,
    isZero,
    isOne,
    isUnit,
    mMap,
    zero,
    zero',
    one,
    one',
    unit
)
where

import qualified Data.List as L


newtype Matrix a = Matrix [[a]] deriving (Eq)

instance Show a => Show (Matrix a) where
    show (Matrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


-- Matrix addition
{-
    Usage:
        mAdd (Matrix [[1,2,3],[4,5,6]]) (Matrix [[1,1,1],[0.2,0.5,0.5]])
        >>> 2.0 3.0 4.0
        >>> 4.2 5.5 6.5
        
        (Matrix [[1,2,3]]) |+| (Matrix [[1,1,1]])
        >>> 2 3 4
-}
mAdd :: Num a => Matrix a -> Matrix a -> Matrix a
mAdd (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (+)) a b


(|+|) (Matrix a) (Matrix b) = mAdd (Matrix a) (Matrix b)


-- Adding a list of matrices
{-
    Usage:
        m1
        >>> 1   0
            0   1

        m4
        >>> 1   6
            2   3

        mAddL [m1,m4]
        >>> 2   6
            2   4
-}
mAddL :: Num a => [Matrix a] -> Matrix a
mAddL = foldl1 mAdd


-- Matrix subtraction
{-
    Usage:
        (Matrix [[1,2,3],[4,5,6]]) |-| (Matrix [[1,1,1],[0,0,0]])
        >>> 0 1 2
        >>> 4 5 6

        mSub (Matrix [[1,2,3],[4,5,0]]) (Matrix [[1,1,1],[0.2,0.5,0.5]])
        >>> 0.0 1.0 2.0
        >>> 3.8 4.5 -0.5
-}
mSub :: Num a => Matrix a -> Matrix a -> Matrix a
mSub (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (-)) a b


(|-|) (Matrix a) (Matrix b) = mSub (Matrix a) (Matrix b)


-- Subtracting a list of matrices
{-
    Usage:
        m1
        >>> 1   0
            0   1

        m4
        >>> 1   6
            2   3

        mSubL [m1,m4]
        >>> 0   -6
            -2  -2
-}
mSubL :: Num a => [Matrix a] -> Matrix a
mSubL = foldl1 mSub


-- Matrix transposition
{-
    Usage:
        mTranspose (Matrix [[1,0,1],[0,1,2]])
        >>> 1 0
        >>> 0 1
        >>> 1 2

        mTranspose (Matrix [[1,0],[2,1],[3,2],[4,3]])
        >>> 1 2 3 4
        >>> 0 1 2 3
-}
mTranspose :: Matrix a -> Matrix a
mTranspose (Matrix []) = Matrix []
mTranspose (Matrix [[]]) = Matrix [[]]
mTranspose xs = Matrix $ foldr (zipWith (:)) (repeat []) (mat2list xs)


-- Multiplication by a scalar
{-
    Usage:
        mScalarMult (-3/2) (Matrix [[1,0,0],[0,1,0],[0,0,1]])
        >>> -1.5 -0.0 -0.0
        >>> -0.0 -1.5 -0.0
        >>> -0.0 -0.0 -1.5

        (-3/2) |*| (Matrix [[1,0,0],[0,1,0],[0,0,1]])
        >>> -1.5 -0.0 -0.0
        >>> -0.0 -1.5 -0.0
        >>> -0.0 -0.0 -1.5

        mScalarMult 3 (Matrix [[1,0,0],[0,1,0],[0,0,1]])
        >>> 3 0 0
        >>> 0 3 0
        >>> 0 0 3
-}
mScalarMult :: Num a => a -> Matrix a -> Matrix a
mScalarMult x (Matrix m) = Matrix $ map (map (x*)) m


(|*|) x (Matrix m) = mScalarMult x (Matrix m)


-- Matrix multiplication
{-
    Usage:
        mMult (Matrix [[1,2,3],[4,5,6]]) (Matrix [[4.5,7,8],[10,(-10),6]])
        >>> 24.5    -13.0       20.0
        >>> 68.0    -22.0       62.0
        
        (Matrix [[1,0],[0,1]]) |><| (Matrix [[4.5,8],[(-10),6]])
        >>> 4.5 8.0
        >>> -10.0   6.0
-}
mMult :: Num a => Matrix a -> Matrix a -> Matrix a
mMult (Matrix m1) (Matrix m2) = Matrix [ map (multRow r) m2t | r <- m1 ]
    where 
        (Matrix m2t) = mTranspose (Matrix m2)
        multRow r1 r2 = sum $ zipWith (*) r1 r2


(|><|) (Matrix a) (Matrix b) = mMult (Matrix a) (Matrix b)


-- Multiplying a list of Matrices
{-
    Usage:
        m1
        >>> 1   0
            0   1

        m4
        >>> 1   6
            2   3

        mMultL [m1,m1,m4]
        >>> 1   6
            2   3
-}
mMultL :: Num a => [Matrix a] -> Matrix a
mMultL = foldl1 mMult


-- Finding number of rows
{-
    Usage:
        numRows (Matrix [[1],[2],[3],[4]])
        >>> 4

        numRows (Matrix [])
        >>> 0

        numRows (Matrix [[]])
        >>> 1

        numRows (Matrix [[1..10],[2,4..20]])
        >>> 2
-}
numRows :: Num a => Matrix a -> Int
numRows (Matrix a) = length a


-- Finding number of columns
{-
    Usage:
        numCols (Matrix [[1],[2],[3],[4]])
        >>> 1

        numCols (Matrix [])
        >>> 0

        numCols (Matrix [[]])
        >>> 1

        numCols (Matrix [[1..10],[2,4..20]])
        >>> 10
-}
numCols :: Num a => Matrix a -> Int
numCols (Matrix a) = numRows (mTranspose (Matrix a))


-- Finding coordinates/position of an element
coords :: Num a => Matrix a -> [[(Int, Int)]]
coords (Matrix a) = zipWith (map . (,)) [0..] $ map (zipWith const [0..]) a

delmatrix :: Num a => Int -> Int -> Matrix a -> Matrix a
delmatrix i j (Matrix a) = Matrix $ dellist i $ map (dellist j) a
    where
        dellist i xs = take i xs ++ drop (i + 1) xs

-- Converting a Matrix into a list
{-
    Usage:
        mat2list (Matrix [[1..10],[2,4..20]])
        >>> [[1,2,3,4,5,6,7,8,9,10],[2,4,6,8,10,12,14,16,18,20]]

        mat2list (Matrix [[1],[2],[3],[4]])
        >>> [[1],[2],[3],[4]]
-}
mat2list :: Matrix a -> [[a]]
mat2list (Matrix m) = m


-- Calculating determinant of a matrix
{-
    Usage:
        determinant (Matrix [[1,0],[0,1]])
        >>> 1.0

        determinant (Matrix [[1,1],[1,(-1)]])
        >>> -2.0

        determinant (Matrix [[2,4,5],[8,(-1.5),6.8]])
        >>> -35.0
-}
--determinant :: [[Double]] -> Double
--determinant :: Matrix a -> Double
determinant (Matrix m)
    | numRows (Matrix m) == 1 = head (head m)
    | otherwise    = sum $ zipWith addition [0..] m
    where
        addition i (x:_) =  x * cofactor i 0 (Matrix m)


-- Calculating cofactor
cofactor :: Int -> Int -> Matrix Double -> Double
cofactor i j (Matrix m) = ((-1.0) ** fromIntegral (i + j)) * determinant (delmatrix i j (Matrix m))


-- Calculating minors
cofactorM :: Matrix Double -> Matrix Double
cofactorM (Matrix m) = Matrix $ map (map (\(i,j) -> cofactor j i (Matrix m))) $ coords (Matrix m)


-- Matrix inversion
{-
    Usage:
        inverse (Matrix [[1,1],[1,(-1)]])
        >>> 0.5 0.5
        >>> 0.5 -0.5

        inverse (Matrix [[2,4,5],[8,(-1.5),6.8]])
        >>> 4.285714285714286e-2 0.11428571428571428 -0.9914285714285714
        >>> 0.22857142857142856 -5.714285714285714e-2 -0.7542857142857142
        
        inverse (Matrix [[1,0],[0,1]])
        >>> 1.0 -0.0
        >>> -0.0 1.0
-}
inverse :: Matrix Double -> Matrix Double
inverse (Matrix m) = Matrix $ map (map (* recip det)) $ mat2list $ cofactorM (Matrix m)
    where
        det = determinant (Matrix m)


-- Matrix division
{-
    Usage:
        mDiv (Matrix [[4.5,6.7],[8.9,10.0]]) (Matrix [[1.0,0.0],[0.0,1.0]])
        >>> 4.5 6.7
        >>> 8.9 10.0

        Matrix [[1.0,0.0],[0.0,1.0]]) |/| (Matrix [[4.5,6.7],[8.9,10.0]])
        >>> -0.6835269993164729 0.45796308954203685
        >>> 0.608339029391661 -0.3075871496924128
-}
mDiv :: Matrix Double -> Matrix Double -> Matrix Double
mDiv (Matrix a) (Matrix b) = mMult (Matrix a) (inverse (Matrix b))


(|/|) (Matrix a) (Matrix b) = mDiv (Matrix a) (Matrix b)


-- Extract particular row of a matrix
{-
    Usage:
        extractRow (Matrix [[1..5]]) 0
        >>> [1,2,3,4,5]

        extractRow (Matrix [[1..5],[6..10],[11..15]]) 2
        >>> [11,12,13,14,15]
-}
extractRow :: Matrix a -> Int -> [a]
extractRow (Matrix m) n = m !! n


-- Extract particular column of a matrix
{-
    Usage:
        extractCol (Matrix [[1..5],[6..10],[11..15]]) 2
        >>> [3,8,13]

        extractCol (Matrix [[1..5]]) 0
        >>> [1]
-}
extractCol :: Matrix a -> Int -> [a]
extractCol (Matrix m) n = mat2list (mTranspose (Matrix m)) !! n


-- Extract range of rows from a matrix
{-
    Usage:
        extractRowRange (Matrix [[1,2],[3,4],[5,6],[7.8,(-9)],[10,11.5]]) 2 4
        >>> 5.0 6.0
        >>> 7.8 -9.0
        >>> 10.0 11.5
-}
extractRowRange :: Matrix a -> Int -> Int -> Matrix a
extractRowRange (Matrix m) a b = Matrix [extractRow (Matrix m) i | i <- [a..b]]


-- Extract range of columns from a matrix
{-
    Usage:
        extractColRange (Matrix [[1,2],[3,4],[5,6],[7.8,(-9)],[10,11.5]]) 0 1
        >>> 1.0 3.0 5.0 7.8 10.0
        >>> 2.0 4.0 6.0 -9.0 11.5
-}
extractColRange :: Matrix a -> Int -> Int -> Matrix a
extractColRange (Matrix m) a b = Matrix [extractCol (Matrix m) i | i <- [a..b]]


-- Power of a matrix
{-
    Usage:
        mPower (Matrix [[1,0],[0,1]]) 100
        >>> 1 0
        >>> 0 1

        mPower (Matrix [[1,1],[2,(-1)]]) 42
        >>> 10460353203 0
        >>> 0 10460353203
-}
--mPower :: Num a => Matrix a -> Int -> Matrix a
mPower (Matrix matrix) exp
    | exp < 0 = mPower (inverse (Matrix matrix)) (-exp)
    | exp == 0 = error "Exponent must be non-zero."
    | exp == 1 = Matrix matrix
    | otherwise = mMult (Matrix matrix) (mPower (Matrix matrix) (exp - 1))


-- Trace of a matrix
{-
    Usage:
        trace (Matrix [[1,1,1],[1,2,3],[4,(-6),7.8]])
        >>> 10.8

        trace (Matrix [[1,0,0],[0,1,0],[0,0,1]])
        >>> 3
-}
trace (Matrix m) = sum [extractRow (Matrix m) r !! c | r <- [0 .. (numRows (Matrix m) - 1)], c <- [0 .. (numCols (Matrix m) - 1)], r == c]


-- Invertibility
{-
    Usage:
        isInvertible (Matrix [[1,1],[1,(-1)]])
        >>> True

        isInvertible (Matrix [[1,1],[(-1),1]])
        >>> True

        isInvertible (Matrix [[1,1],[1,1]])
        >>> False

        isInvertible (Matrix [[1,0],[0,1]])
        >>> True
-}
--isInvertible :: Num a => Matrix a -> Bool
isInvertible (Matrix m) = determinant (Matrix m) /= 0


-- Is it symmetric?
{-
    Usage:
        isSymmetric (Matrix [[1,2,3],[2,1,3],[3,3,4]])
        >>> True

        isSymmetric (Matrix [[1,0],[0,1]])
        >>> True

        isSymmetric (Matrix [[1,1],[(-1),1]])
        >>> False
-}
--isSymmetric :: Eq a => Matrix a -> Bool
isSymmetric (Matrix m) = Matrix m == mTranspose (Matrix m)


-- Is it anti/skew-symmetric?
{-
    Usage:
        isSkewSymmetric (Matrix [[0,0],[0,0]])
        >>> True
        
        isSkewSymmetric (Matrix [[1,1],[(-1),1.5]])
        >>> False
-}
--isSkewSymmetric :: Eq a => Matrix a -> Bool
isSkewSymmetric (Matrix m) = Matrix m == mScalarMult (-1) (mTranspose (Matrix m))


-- Is it a row matrix?
{-
    Usage:
        isRow (Matrix [[1..5]])
        >>> True

        isRow (Matrix [[1..5],[2..7]])
        >>> False
-}
--isRow :: Ord a => Matrix a -> Bool
isRow (Matrix m) = numRows (Matrix m) == 1


-- Is it a column matrix?
{-
    Usage:
        isColumn (Matrix [[1..4],[2..6]])
        >>> False

        isColumn (Matrix [[1],[2],[3]])
        >>> True
-}
--isColumn :: Ord a => Matrix a -> Bool
isColumn (Matrix m) = numCols (Matrix m) == 1


-- Is it a square matrix?
{-
    Usage:
        isSquare (Matrix [[1,0,0],[1,2,3]])
        >>> False

        isSquare (Matrix [[1,0],[0,1]])
        >>> True
-}
--isSquare :: Ord a => Matrix a -> Bool
isSquare (Matrix m) = numRows (Matrix m) == numCols (Matrix m)


-- Orthogonality
{-
    Usage:
        isOrthogonal (Matrix [[1,0],[0,1]])
        >>> True

        isOrthogonal (Matrix [[1,1],[1.2,(-1.5)]])
        >>> False
-}
--isOrthogonal :: Eq a => Matrix a -> Bool
isOrthogonal (Matrix m) = mTranspose (Matrix m) == inverse (Matrix m)


-- Is it an involutive matrix?
{-
    Usage:
        isInvolutive (Matrix [[1,0],[0,1]])
        >>> True

        isInvolutive (Matrix [[1,2],[2,1]])
        >>> False
-}
--isInvolutive :: Eq a => Matrix a -> Bool
isInvolutive (Matrix m) = Matrix m == inverse (Matrix m)


-- Is it a 0/1 Matrix?
{-
    Usage:
        isZeroOne (Matrix [[1,1],[1,4]])
        >>> False

        isZeroOne (Matrix [[1,1],[1,0]])
        >>> True
-}
--isZeroOne :: Ord a => Matrix a -> Bool
isZeroOne (Matrix m) = and [(((m!!r)!!c) == 0) || (((m!!r)!!c) == 1) | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


-- Is it a zero matrix?
{-
    Usage:
        isZero (Matrix [[0,0,0]])
        >>> True

        isZero (Matrix [[0,1],[1,2]])
        >>> False
-}
--isZero :: Ord a => Matrix a -> Bool
isZero (Matrix m) = and [(m!!r)!!c == 0 | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


-- Is it a one matrix?
{-
    Usage:
        isOne (Matrix [[1,1],[1,1]])
        >>> True

        isOne (Matrix [[1,1],[1,0]])
        >>> False
-}
--isOne :: Ord a => Matrix a -> Bool
isOne (Matrix m) = and [(m!!r)!!c == 1 | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


-- Is it a unit matrix?
{-
    Usage:
        isUnit (Matrix [[1,0],[0,1]])
        >>> True

        isUnit (Matrix [[3,0],[0,3]])
        >>> False

        isUnit (Matrix [[1,0,0],[0,1,0],[0,0,1]])
        >>> True
        
        isUnit (Matrix [[]])
        >>> False

        isUnit (Matrix [[1]])
        >>> True
-}
--isUnit :: Eq a => Matrix a -> Bool
isUnit (Matrix [[]]) = False
isUnit (Matrix [[1]]) = True
isUnit (Matrix m) = and ([isSquare (Matrix m)] ++ [isOrthogonal (Matrix m)] ++ [isSymmetric (Matrix m)] ++ [trace (Matrix m) == fromIntegral (numRows (Matrix m))])


-- Mapping a function to a matrix
{-
    Usage:
        m
        >>> 1.0 0.0 2.0
            2.0 5.0 6.5

        mMap (*3) m
        >>> 3.0 0.0 6.0
            6.0 15.0    19.5
-}
mMap f (Matrix m) = Matrix $ map (map f) m


-- Generate special matrices

-- Temp function (converts list to n-row matrix)
chunk' n = takeWhile (not.null) . map (take n) . iterate (drop n)


-- NxN 0 matrix
zero n = Matrix $ chunk' n (replicate (n * n) 0)


-- MxN 0 matrix
zero' m n = Matrix $ chunk' m (replicate (m * n) 0)


-- NxN 1 matrix
one n = Matrix $ chunk' n (replicate (n * n) 1)


-- MxN 1 matrix
one' m n = Matrix $ chunk' m (replicate (m * n) 1)


-- NxN unit matrix
unit n = Matrix $ chunk' n (L.intercalate (replicate n 0) (mat2list (one' 1 n)))


-- SAMPLE MATRICES --
m1 = Matrix [[1,0],[0,1]]

m2 = Matrix [[1,2,3],[4,5,6],[7,8,9],[10.5,11,12]]

m3 = Matrix [[]]
