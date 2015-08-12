{-|
Module      : Matrix
Description : Matrix module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for:
    * Matrix addition
    * Matrix subtraction
    * Matrix multiplication
    * Multiplication with a scalar
    * Matrix division
    * Matrix transposition
    * Matrix inversion
    * Finding number of rows and columns
    * Matrix equality
    * Extractig row/s and column/s
    * Power of matrices
    * Trace of a matrix
    * Checking for type of matrix (symmetric, skew-symmetric, row, column, square, zero, one, unit/identity, orthogonal, involutive, invertible)    
    * Determinant
    * Calculating determinant of a matrix
    * Finding minors and cofactors

-}

module Matrix
(
    Matrix(..),
    mAdd,
    mAddL,
    (|+|),
    mSub,
    mSubL,
    (|-|),
    transpose,
    mScalarMult,
    (|*|),
    mMult,
    mMultL,
    (|><|),
    numRows,
    numCols,
    matrixToList,
    listToMatrix,
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

{-|
    'Matrix' is the data type used for representing a matrix as a two-dimensional list.

    For example:

    >>> let m = Matrix [[1,2,3],[4,5,6],[7,8,9]]
    >>> m
    1   2   3
    4   5   6
    7   8   9
-}
newtype Matrix a = Matrix [[a]] deriving (Eq)

instance Show a => Show (Matrix a) where
    show (Matrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


{-|
    The 'mAdd' function adds two matrices, each of type 'Matrix', and returns their sum as a 'Matrix'.

    For example:

    >>> mAdd (Matrix [[1,2,3],[4,5,6]]) (Matrix [[1,1,1],[0.2,0.5,0.5]])
    2.0     3.0     4.0
    4.2     5.5     6.5
-}
mAdd :: Num a => Matrix a -> Matrix a -> Matrix a
mAdd (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (+)) a b


{-|
    The '|+|' operator can also be used for adding two matrices.

    For example:

    >>> (Matrix [[1,2,3]]) |+| (Matrix [[1,1,1]])
    2   3   4
-}
(|+|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|+|) (Matrix a) (Matrix b) = mAdd (Matrix a) (Matrix b)


{-|
    The 'mAddL' function can be used to add a list of matrices, each of type 'Matrix'.

    For example:
  
    >>> m1
    1   0
    0   1

    >>> m4
    1   6
    2   3

    >>> mAddL [m1,m4]
    2   6
    2   4
-}
mAddL :: Num a => [Matrix a] -> Matrix a
mAddL = foldl1 mAdd


{-|
    The 'mSub' function is used to subtract one matrix from another, both of type 'Matrix' and return their difference as a Matrix.

    For example:

    mSub (Matrix [[1,2,3],[4,5,0]]) (Matrix [[1,1,1],[0.2,0.5,0.5]])
    >>> 0.0 1.0 2.0
    >>> 3.8 4.5 -0.5
-}
mSub :: Num a => Matrix a -> Matrix a -> Matrix a
mSub (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (-)) a b


{-|
    The '|-|' can also be used to find the difference between two matrices.

    For example:

    (Matrix [[1,2,3],[4,5,6]]) |-| (Matrix [[1,1,1],[0,0,0]])
    >>> 0 1 2
    >>> 4 5 6
-}
(|-|) (Matrix a) (Matrix b) = mSub (Matrix a) (Matrix b)


{-|
    The 'mSubL' function is used to subtract a list of matrices.

    For example:
    
    >>> m1
    1   0
    0   1

    >>> m4
    1   6
    2   3

    >>> mSubL [m1,m4]
    0   -6
    -2  -2
-}
mSubL :: Num a => [Matrix a] -> Matrix a
mSubL = foldl1 mSub


{-|
    The 'transpose' function returns the transpose of a matrix.

    For example:
    
    >>> transpose (Matrix [[1,0,1],[0,1,2]])
    1 0
    0 1
    1 2

    >>> transpose (Matrix [[1,0],[2,1],[3,2],[4,3]])
    1 2 3 4
    0 1 2 3
-}
transpose :: Matrix a -> Matrix a
transpose (Matrix []) = Matrix []
transpose (Matrix [[]]) = Matrix [[]]
transpose xs = Matrix $ foldr (zipWith (:)) (repeat []) (matrixToList xs)


{-|
    The 'mScalarMult' function is used to multiply a scalar value and a matrix .

    For example:

    >>> mScalarMult (-3/2) (Matrix [[1,0,0],[0,1,0],[0,0,1]])
    -1.5 -0.0 -0.0
    -0.0 -1.5 -0.0
    -0.0 -0.0 -1.5

    >>> mScalarMult 3 (Matrix [[1,0,0],[0,1,0],[0,0,1]])
    3 0 0
    0 3 0
    0 0 3
-}
mScalarMult :: Num a => a -> Matrix a -> Matrix a
mScalarMult x (Matrix m) = Matrix $ map (map (x*)) m


{-|
    The '|*|' operator can also be used to multiply a scalar and a matrix.

    For example:

    >>> (-3/2) |*| (Matrix [[1,0,0],[0,1,0],[0,0,1]])
    -1.5 -0.0 -0.0
    -0.0 -1.5 -0.0
    -0.0 -0.0 -1.5
-}
(|*|) x (Matrix m) = mScalarMult x (Matrix m)


{-|
    The 'mMult' function multiplies two matrices and returns the product.

    For example:
    
    >>> mMult (Matrix [[1,2,3],[4,5,6]]) (Matrix [[4.5,7,8],[10,(-10),6]])
    24.5    -13.0       20.0
    68.0    -22.0       62.0
-}
mMult :: Num a => Matrix a -> Matrix a -> Matrix a
mMult (Matrix m1) (Matrix m2) = Matrix [ map (multRow r) m2t | r <- m1 ]
    where 
        (Matrix m2t) = transpose (Matrix m2)
        multRow r1 r2 = sum $ zipWith (*) r1 r2


{-|
    The '|><|' function can also be used to multiply two matrices.

    For example:

    >>> (Matrix [[1,0],[0,1]]) |><| (Matrix [[4.5,8],[(-10),6]])
    4.5 8.0
    -10.0   6.0
-}
(|><|) (Matrix a) (Matrix b) = mMult (Matrix a) (Matrix b)


{-|
    The 'mMultL' function multiplies a list of matrices and returns their product.

    For example:
    
    >>> m1
    1   0
    0   1

    >>> m4
    1   6
    2   3

    >>> mMultL [m1,m1,m4]
    1   6
    2   3
-}
mMultL :: Num a => [Matrix a] -> Matrix a
mMultL = foldl1 mMult


{-|
    The 'numRows' function returns the number of rows in a specified matrix..

    For example:
    
    >>> numRows (Matrix [[1],[2],[3],[4]])
    4

    >>> numRows (Matrix [])
    0

    >>> numRows (Matrix [[]])
    1

    >>> numRows (Matrix [[1..10],[2,4..20]])
    2
-}
numRows :: Num a => Matrix a -> Int
numRows (Matrix a) = length a


{-|
    The 'numCols' function returns the number of columns in a specified matrix.

    For example:

    >>> numCols (Matrix [[1],[2],[3],[4]])
    1

    >>> numCols (Matrix [])
    0

    >>> numCols (Matrix [[]])
    1

    >>> numCols (Matrix [[1..10],[2,4..20]])
    10
-}
numCols :: Num a => Matrix a -> Int
numCols (Matrix a) = numRows (transpose (Matrix a))


{--
    The 'coords' function returns the row and column indices for all the elements in a matrix.

    For example:

    >>> m1
    1   0
    0   1

    >>> coords m1
    [[(0,0),(0,1)],[(1,0),(1,1)]]
-}
coords :: Num a => Matrix a -> [[(Integer, Integer)]]
coords (Matrix a) = zipWith (map . (,)) [0..] $ map (zipWith const [0..]) a


{-|
    The 'matrixToList' function converts a 'Matrix' into a two-dimensional list.

    For example:
    
    >>> matrixToList (Matrix [[1..10],[2,4..20]])
    [[1,2,3,4,5,6,7,8,9,10],[2,4,6,8,10,12,14,16,18,20]]

    >>> matrixToList (Matrix [[1],[2],[3],[4]])
    [[1],[2],[3],[4]]
-}
matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix m) = m


{-|
    The 'listToMatrix' functionc converts a two-dimensional list into a 'Matrix'.

    For example:

    >>> listToMatrix [[1,2],[3,4]]
    1   2
    3   4
-}
listToMatrix :: [[a]] -> Matrix a
listToMatrix m = Matrix m


{-|
    The 'determinant' function returns the determinant of a matrix.

    For example:
        
    >>> determinant (Matrix [[1,0],[0,1]])
    1.0

    >>> determinant (Matrix [[1,1],[1,(-1)]])
    -2

    >>> determinant (Matrix [[2,4,5],[8,(-1.5),6.8]])
    -35.0
-}
determinant :: Num a => Matrix a -> a
determinant (Matrix m)
    | numRows (Matrix m) == 1 = head (head m)
    | otherwise    = sum $ zipWith addition [0..] m
    where
        addition i (x:_) =  x * cofactor i 0 (Matrix m)


-- internal function
delmatrix :: Num a => Integer -> Integer -> Matrix a -> Matrix a
delmatrix i j (Matrix a) = Matrix $ dellist (fromInteger i) $ map (dellist (fromInteger j)) a
    where
        dellist i xs = take i xs ++ drop (i + 1) xs


-- Calculating cofactor
cofactor :: Num a => Integer -> Integer -> Matrix a -> a
cofactor i j (Matrix m) = ((-1) ^ fromIntegral (i + j)) * determinant (delmatrix i j (Matrix m))


-- Calculating cofactor matrix (minors)
cofactorM :: Num a => Matrix a -> Matrix a
cofactorM (Matrix m) = Matrix $ map (map (\(i,j) -> cofactor j i (Matrix m))) $ coords (Matrix m)


{-|
    The 'inverse' function returns the inverse of a specified matrix.

    For example:
    
    >>> let m5 = Matrix [[4,3],[3,2]]
    >>> inverse m5
    -2.0    3.0
    3.0     -4.0

    >>> let m6 = Matrix [[1,2,3],[0,4,5],[1,0,6]]
    >>> inverse m6
    1.0909090909090908      -0.5454545454545454     -9.090909090909091e-2
    0.2272727272727273      0.13636363636363635     -0.2272727272727273
    -0.18181818181818182    9.090909090909091e-2    0.18181818181818182

    >>> inverse (Matrix [[1,0],[0,1]])
    1.0     -0.0
    -0.0    1.0

    >>> inverse (Matrix [[4.6,3.5],[3.1,2]])
    -1.212121212121212  2.121212121212121
    1.8787878787878785  -2.787878787878787
-}
inverse :: (Eq a, Fractional a) => Matrix a -> Matrix a
inverse (Matrix m) = mScalarMult (reciprocal (determinant (Matrix m))) (cofactorM (Matrix m))


-- Internal function used to calculate the reciprocal of a value.
-- This function also checks if the determinant is 0 (i.e. the matrix is not invertible).
reciprocal :: (Eq a, Fractional a) => a -> a
reciprocal a
    | a /= 0 = 1 / a
    | otherwise = error "The determinant of the matrix is 0. This matrix is not inertible."


{-|
    The 'mDiv' function is used to divide a matrix by another.

    For example:
        
    >>> mDiv (Matrix [[4.5,6.7],[8.9,10.0]]) (Matrix [[1.0,0.0],[0.0,1.0]])
    4.5 6.7
    8.9 10.0
-}
--mDiv :: Matrix Double -> Matrix Double -> Matrix Double
mDiv (Matrix a) (Matrix b) = mMult (Matrix a) (inverse (Matrix b))


{-|
    The '|/|' operator can also be used to divide two matrices.

    For example:

    >>> Matrix [[1.0,0.0],[0.0,1.0]]) |/| (Matrix [[4.5,6.7],[8.9,10.0]])
    -0.6835269993164729 0.45796308954203685
    0.608339029391661 -0.3075871496924128
-}
(|/|) (Matrix a) (Matrix b) = mDiv (Matrix a) (Matrix b)


{-|
    The 'extractRow' function returns a particular row of the matrix passed as argument.
    
    For example:

    >>> extractRow (Matrix [[1..5]]) 0
    [1,2,3,4,5]

    >>> extractRow (Matrix [[1..5],[6..10],[11..15]]) 2
    [11,12,13,14,15]
-}
extractRow :: Matrix a -> Int -> [a]
extractRow (Matrix m) n = m !! n


{-|
    The 'extractCol' function returns a particular column of the matrix passed as argument.
    
    For example:

    >>> extractCol (Matrix [[1..5],[6..10],[11..15]]) 2
    [3,8,13]

    >>> extractCol (Matrix [[1..5]]) 0
    [1]
-}
extractCol :: Matrix a -> Int -> [a]
extractCol (Matrix m) n = matrixToList (transpose (Matrix m)) !! n


{-|
    The 'extractRowRange' function returns a range of rows from the passed matrix.

    For example:

    >>> extractRowRange (Matrix [[1,2],[3,4],[5,6],[7.8,(-9)],[10,11.5]]) 2 4
    5.0     6.0
    7.8     -9.0
    10.0    11.5
-}
extractRowRange :: Matrix a -> Int -> Int -> Matrix a
extractRowRange (Matrix m) a b = Matrix [extractRow (Matrix m) i | i <- [a..b]]


{-|
    The 'extractColRange' function returns a range of columns from the passed matrix.

    For example:

    >>> extractColRange (Matrix [[1,2],[3,4],[5,6],[7.8,(-9)],[10,11.5]]) 0 1
    1.0 3.0 5.0 7.8 10.0
    2.0 4.0 6.0 -9.0 11.5
-}
extractColRange :: Matrix a -> Int -> Int -> Matrix a
extractColRange (Matrix m) a b = Matrix [extractCol (Matrix m) i | i <- [a..b]]


{-|
    The 'mPower' function returns the power of a matrix.

    For example:

    >>> m5
    4.6 3.5
    3.1 2.0
    >>> mPower m5 10
    1.5349250119379473e8    1.1096944577877794e8
    9.828722340406047e7     7.105805575813112e7

    >>> mPower (unit 3) 3
    1.0 0.0 0.0
    0.0 1.0 0.0
    0.0 0.0 1.0

    >>> mPower (unit 3) (-3)
    1.0 0.0 0.0
    0.0 1.0 0.0
    0.0 0.0 1.0
-}
mPower :: (Eq a1, Fractional a1, Num a, Ord a) => Matrix a1 -> a -> Matrix a1
mPower (Matrix matrix) exp
    | exp < 0 = mPower (inverse (Matrix matrix)) (-exp)
    | exp == 0 = error "Exponent must be non-zero."
    | exp == 1 = Matrix matrix
    | otherwise = mMult (Matrix matrix) (mPower (Matrix matrix) (exp - 1))


{-|
    The 'trace' function returns the trace of a matrix.

    For example:

    >>> trace (Matrix [[1,1,1],[1,2,3],[4,(-6),7.8]])
    10.8

    >>> trace (Matrix [[1,0,0],[0,1,0],[0,0,1]])
    3
-}
trace :: Num a => Matrix a -> a
trace (Matrix m) = sum [extractRow (Matrix m) r !! c | r <- [0 .. (numRows (Matrix m) - 1)], c <- [0 .. (numCols (Matrix m) - 1)], r == c]


{-|
    The 'isInvertible' function checks if a matrix can be inverted or not.

    For example:

    >>> isInvertible (Matrix [[1,1],[1,(-1)]])
    True

    >>> isInvertible (Matrix [[1,1],[(-1),1]])
    True

    >>> isInvertible (Matrix [[1,1],[1,1]])
    False

    >>> isInvertible (Matrix [[1,0],[0,1]])
    True
-}
isInvertible :: (Eq a, Num a) => Matrix a -> Bool
isInvertible (Matrix m) = determinant (Matrix m) /= 0


{-|
    The 'isSymmetric' function checks if a matrix is symmetric.

    For example:
    
    >>> isSymmetric (Matrix [[1,2,3],[2,1,3],[3,3,4]])
    True

    >>> isSymmetric (Matrix [[1,0],[0,1]])
    True

    >>> isSymmetric (Matrix [[1,1],[(-1),1]])
    False
-}
isSymmetric :: Eq a => Matrix a -> Bool
isSymmetric (Matrix m) = Matrix m == transpose (Matrix m)


{-|
    The 'isSkewSymmetric' function checks if a matrix is anti/skew-symmetric.

    For example:

    >>> isSkewSymmetric (Matrix [[0,0],[0,0]])
    True
    
    >>> isSkewSymmetric (Matrix [[1,1],[(-1),1.5]])
    False
-}
isSkewSymmetric :: (Eq a, Num a) => Matrix a -> Bool
isSkewSymmetric (Matrix m) = Matrix m == mScalarMult (-1) (transpose (Matrix m))


{-|
    The 'isRow' function checks if a matrix is a row matrix.

    For example:

    >>> isRow (Matrix [[1..5]])
    True

    >>> isRow (Matrix [[1..5],[2..7]])
    False
-}
isRow :: Num a => Matrix a -> Bool
isRow (Matrix m) = numRows (Matrix m) == 1


{-|
    The 'isColumn' function checks if a matrix is a column matrix.

    For example:

    >>> isColumn (Matrix [[1..4],[2..6]])
    False

    >>> isColumn (Matrix [[1],[2],[3]])
    True
-}
isColumn :: Num a => Matrix a -> Bool
isColumn (Matrix m) = numCols (Matrix m) == 1


{-|
    The 'isSquare' matrix checks if a matrix is a square matrix.

    For example:
        
    >>> isSquare (Matrix [[1,0,0],[1,2,3]])
    False

    >>> isSquare (Matrix [[1,0],[0,1]])
    True
-}
isSquare :: Num a => Matrix a -> Bool
isSquare (Matrix m) = numRows (Matrix m) == numCols (Matrix m)


{-|
    The 'isOrthogonal' function checks if a matrix is orthogonal.

    For example:

    >>> isOrthogonal (Matrix [[1,0],[0,1]])
    True

    >>> isOrthogonal (Matrix [[1,1],[1.2,(-1.5)]])
    False
-}
isOrthogonal :: (Eq a, Fractional a) => Matrix a -> Bool
isOrthogonal (Matrix m) = transpose (Matrix m) == inverse (Matrix m)


{-|
    The 'isInvolutive' function checks if a matrix is involutive.

    For example:

    >>> isInvolutive (Matrix [[1,0],[0,1]])
    True

    >>> isInvolutive (Matrix [[1,2],[2,1]])
    False
-}
isInvolutive :: (Eq a, Fractional a) => Matrix a -> Bool
isInvolutive (Matrix m) = Matrix m == inverse (Matrix m)


{-|
    The 'isZeroOne' function returns true if a matrix is a zero or one matrix.

    For example:

    >>> isZeroOne (Matrix [[1,1],[1,4]])
    False

    >>> isZeroOne (Matrix [[1,1],[1,0]])
    True
-}
isZeroOne :: (Eq a, Num a) => Matrix a -> Bool
isZeroOne (Matrix m) = and [(((m!!r)!!c) == 0) || (((m!!r)!!c) == 1) | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


{-|
    The 'isZero' function returns true if a matrix is a zero matrix (all elements evaluate to 0).

    For example:

    >>> isZero (Matrix [[0,0,0]])
    True

    >>> isZero (Matrix [[0,1],[1,2]])
    False
-}
isZero :: (Eq a, Num a) => Matrix a -> Bool
isZero (Matrix m) = and [(m!!r)!!c == 0 | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


{-|
    The 'isOne' function returns true if a matrix is a one matrix (all elements evaluate to 1).
    Usage:
        isOne (Matrix [[1,1],[1,1]])
        >>> True

        isOne (Matrix [[1,1],[1,0]])
        >>> False
-}
isOne :: (Eq a, Num a) => Matrix a -> Bool
isOne (Matrix m) = and [(m!!r)!!c == 1 | r <- [0..(numRows (Matrix m) - 1)], c <- [0..(numCols (Matrix m) - 1)]]


{-|
    The 'isUnit' function returns true is a matrix is a unit matrix.

    For example:
        
    >>> isUnit (Matrix [[1,0],[0,1]])
    True

    >>> isUnit (Matrix [[3,0],[0,3]])
    False

    >>> isUnit (Matrix [[1,0,0],[0,1,0],[0,0,1]])
    True
    
    >>> isUnit (Matrix [[]])
    False

    >>> isUnit (Matrix [[1]])
    True
-}
isUnit :: (Eq a, Fractional a) => Matrix a -> Bool
isUnit (Matrix [[]]) = False
isUnit (Matrix [[1]]) = True
isUnit (Matrix m) = and ([isSquare (Matrix m)] ++ [isOrthogonal (Matrix m)] ++ [isSymmetric (Matrix m)] ++ [trace (Matrix m) == fromIntegral (numRows (Matrix m))])


{-|
    The 'mMap' function maps a function to a matrix, i.e. applies a function to all elements of a matrix.

    For example:
    
    >>> m
    1.0 0.0 2.0
    2.0 5.0 6.5

    >>> mMap (*3) m
    3.0     0.0     6.0
    6.0     15.0    19.5
-}
mMap :: (a1 -> a) -> Matrix a1 -> Matrix a
mMap f (Matrix m) = Matrix $ map (map f) m


-- Temp function (converts list to n-row matrix)
chunk' n = takeWhile (not.null) . map (take n) . iterate (drop n)


{-|
    The 'zero' function returns a zero matrix of order 'n x n', where n is the argument passed.

    For example:

    >>> zero 2
    0   0
    0   0

    >>> zero 3
    0   0   0
    0   0   0
    0   0   0
-}
zero :: Num a => Int -> Matrix a
zero n = Matrix $ chunk' n (replicate (n * n) 0)


{-|
    The 'zero'' function returns a zero matrix of order 'm x n', where m and n are the arguments passed.

    For example:

    >>> zero 2 3
    0   0   0
    0   0   0

    >>> zero 3 3
    0   0   0
    0   0   0
    0   0   0
-}
zero' :: Num a => Int -> Int -> Matrix a
zero' m n = Matrix $ chunk' m (replicate (m * n) 0)


{-|
    The 'one' function returns a one matrix of order 'n x n', where n is the argument passed.

    For example:

    >>> one 2
    1   1
    1   1

    >>> one 3
    1   1   1
    1   1   1
    1   1   1
-}
one :: Num a => Int -> Matrix a
one n = Matrix $ chunk' n (replicate (n * n) 1)


{-|
    The 'one'' function returns a one matrix of order 'm x n', where m and n are the arguments passed.

    For example:

    >>> one 2 2
    1   1
    1   1

    >>> one 3 2
    1   1
    1   1
    1   1
-}
one' :: Num a => Int -> Int -> Matrix a
one' m n = Matrix $ chunk' m (replicate (m * n) 1)


{-|
    The 'unit' function returns a unit matrix of order 'n x n', where n is the argument passed.

    For example:

    >>> unit 2
    1   0
    0   1

    >>> unit 3
    1   0   0
    0   1   0
    0   0   1
-}
unit :: Num a => Int -> Matrix a
unit n = Matrix $ chunk' n (L.intercalate (replicate n 0) (matrixToList (one' 1 n)))


-- SAMPLE MATRICES --
m1 = Matrix [[1,0],[0,1]]

m2 = Matrix [[1,2,3],[4,5,6],[7,8,9],[10.5,11,12]]

m3 = Matrix [[]]
