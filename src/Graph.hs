{-|
Module 		  : Graph
Description : Graph module for the MPL DSL
Copyright	  : (c) Rohit Jha, 2015
License     : BSD2
Maintainer	: rohit305jha@gmail.com
Stability	  : Stable

Functionality for:
	* Data types (Graph, GraphMatrix, Edges and Vertices)
	* Adding edges and vertices
	* Getting vertices and their number
	* Getting edges and their number
	* Converting between different Graph data types
	* Transpose
	* Checking for directedness
	* Union
	* Connectedness of vertices
	* Number of paths between two vertices
	* Adjacent nodes of a vertex
	* Degrees of vertices
	* Euler circuits and paths
	* Hamiltonian circuits
-}

module Graph
(
  Vertices(..),
  Edges(..),
  Graph(..),
  GraphMatrix(..),
  verticesToList,
  listToVertices,
  edgesToList,
  listToEdges,
  graphToMatrix,
  matrixToGraph,
  getVerticesG,
  getVerticesGM,
  numVerticesG,
  numVerticesGM,
  getEdgesG,
  getEdgesGM,
  numEdgesG,
  numEdgesGM,
  convertGM2G,
  convertG2GM,
  transposeG,
  transposeGM,
  isUndirectedG,
  isUndirectedGM,
  isDirectedG,
  isDirectedGM,
  unionG,
  unionGM,
  addVerticesG,
  addVerticesGM,
  getVerticesFromEdges,
  addEdgesG,
  addEdgesGM,
  areConnectedGM,
  numPathsBetweenGM,
  adjacentNodesG,
  adjacentNodesGM,
  inDegreeG,
  inDegreeGM,
  outDegreeG,
  outDegreeGM,
  degreeG,
  degreeGM,
  hasEulerCircuitG,
  hasEulerCircuitGM,
  hasEulerPathG,
  hasEulerPathGM,
  hasHamiltonianCircuitG,
  hasHamiltonianCircuitGM,
  countOddDegreeV,
  countEvenDegreeV,
  hasEulerPathNotCircuitG,
  hasEulerPathNotCircuitGM,
  isSubgraphG,
  isSubgraphGM
)
where

import qualified Data.List as L


{-|
  	'Vertices' is a data type for representing the vertices of a graph.
  	It is the set of all the vertices in a graph.
  	'Vertices' are internally stored as a list.

  	Below are a few examples:
  	
  	>>> Vertices []
  	{}
  	
  	>>> Vertices [1,2,5]
  	{1,2,5}

  	>>> Vertices [1..10]
    {1,2,3,4,5,6,7,8,9,10}
-}
newtype Vertices a = Vertices [a] deriving (Eq)

instance (Show a) => Show (Vertices a) where
  showsPrec _ (Vertices s) = showVertices s

showVertices [] str = showString "{}" str
showVertices (x:xs) str = showChar '{' (shows x (showl xs str))
  where 
    showl [] str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

{-|
  	The 'verticesToList' function returns the list of vertices of a graph represented as 'Vertices'.

  	Below is an example:

  	>>> v1
  	{1,2,3,4}

  	>>> verticesToList v1
    [1,2,3,4]
-} 
verticesToList :: Vertices t -> [t]
verticesToList (Vertices v) = v


{-|
    The 'list2vertices' function returns a 'Vertices' from a list of vertices.

    For example:

    >>> list2vertices [1,2,3,4]
    {1,2,3,4}
-}
listToVertices :: [t] -> Vertices t
listToVertices v = Vertices v


{-|
  	'Edges' is a data types for representing the edges of a graph.
  	It is the set of all edges in a graph, including their starting and ending vertices, and the weight of the edge.

  	Below are a few examples:
  	
  	>>> Edges [(1,1,10)]
  	{(1,1,10)}

  	>>> Edges [(1,2,1),(1,3,5),(2,3,2)]
  	{(1,2,1),(1,3,5),(2,3,2)}

  	>>> Edges []
  	{}
-}
newtype Edges a = Edges [(a,a,Int)] deriving (Eq)

instance (Show a) => Show (Edges a) where
  showsPrec _ (Edges s) = showEdges s

showEdges [] str = showString "{}" str
showEdges (x:xs) str = showChar '{' (shows x (showl xs str))
  where 
    showl [] str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

{-|
  	The 'edgesToList' function returns the list of edges of a graph represented as 'Edges'.

  	Below is an example:

  	>>> e1
  	{(1,2,4),(1,3,1),(1,4,5),(3,2,6)}

  	>>> edgesToList e1
  	[(1,2,4),(1,3,1),(1,4,5),(3,2,6)]
-}
edgesToList :: Edges t -> [(t, t, Int)]
edgesToList (Edges a) = a


{-|
    The 'listToEdges' function converts a list of triples into 'Edges'.

    For example:

    >>> l
    [(1,2,1),(2,2,4)]

    >>> listToEdges l
    {(1,2,1),(2,2,4)}
-}
listToEdges :: [(a, a, Int)] -> Edges a
listToEdges e = Edges e


-- internal functions
first (a,b,c) = a
second (a,b,c) = b
third (a,b,c) = c


{-|
    'Graph' is a data type for representing graphs as a group of 'Vertices' and 'Edges'.

    For example:

	>>> let v = Vertices [1,2,3,4]
    >>> v
    {1,2,3,4}
		
    >>> let e = Edges [(1,2,5),(1,3,7),(2,4,3)]
    >>> e
    {(1,2,5),(1,3,7),(2,4,3)}
		
    >>> let g = Graph (v,e)
    >>> g
    Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})
-}
newtype Graph a = Graph (Vertices a, Edges a) deriving (Eq, Show)


-- Internal type used for representing GraphMatrix
newtype Matrix a = Matrix [[a]] deriving (Eq)

instance Show a => Show (Matrix a) where
  show (Matrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


{-|
    The 'GraphMatrix' data type is used here to represent a 'Graph' as a two-dimentional list.

    For example:
    
    >>> GraphMatrix [[1,2,5],[3,5,7],[0,0,4]]
    1 2 5
    3 5 7
    0 0 4
-}
newtype GraphMatrix a = GraphMatrix [[a]] deriving (Eq)

instance Show a => Show (GraphMatrix a) where
  show (GraphMatrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


{-|
    The 'graphToMatrix' function converts 'GraphMatrix' type to a two-dimentional list.

    For example:

    >>> gm1
    0 1 3 5
    4 0 2 6
    1 2 3 4
    0 0 1 6
    
    >>> graphToMatrix gm1
    [[0,1,3,5],[4,0,2,6],[1,2,3,4],[0,0,1,6]]
-}
graphToMatrix :: GraphMatrix a -> [[a]]
graphToMatrix (GraphMatrix gm) = gm


{-|
    The 'matrixToGraph' function converts a two-dimensional list to a 'GraphMatrix'.

    For example:

    >>> m
    [[0,1],[4,0]]
    
    >>> graphToMatrix m
    0   1
    4   0
-}
matrixToGraph :: [[a]] -> GraphMatrix a
matrixToGraph m = GraphMatrix m


{-|
    The 'getVerticesG' function returns the set of all vertices from a graph.
    The function takes one argument, which is of type 'Graph'.

    For example:

	>>>g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> getVerticesG g
	{1,2,3,4}
-}
getVerticesG :: Num a => Graph a -> Vertices a
getVerticesG (Graph g) = fst g


{-|
    The 'numVerticesG' function returns the number of vertices in a graph.
    The function takes one argument, which is of type 'Graph'.

    For example:
		
    >>> g
    Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> numVerticesG g
	4
-}
numVerticesG :: Num a => Graph a -> Int
numVerticesG (Graph g) = length $ verticesToList $ getVerticesG (Graph g)


{-|
    The 'numEdgesG' function returns the number of edges in a graph.
    The function takes one argument, which is of type 'Graph'.

    For example:

	>>> g
    Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> numEdgesG g
	3
-}
numEdgesG :: Num a => Graph a -> Int
numEdgesG (Graph g) = length $ edgesToList $ getEdgesG (Graph g)


{-|
    The 'getEdgesG' function returns the edges of a graph.
    The function takes one argument, which is of type 'Graph'.

    For example:

	>>> g
    Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> getEdgesG g
	{(1,2,5),(1,3,7),(2,4,3)}
-}
getEdgesG :: Num a => Graph a -> Edges a
getEdgesG (Graph g) = snd g


{-|
    The 'getVerticesGM' returns the edges in a graph from its adjacency matrix representation.
    The function takes one argument, which is of type 'GraphMatrix'.

    For example:

	>>> gm
    1	2	5
    3	5	7
    0	0	4

	>>> getVerticesGM gm
	{1,2,3}
-}
getVerticesGM :: GraphMatrix t -> Vertices Int
getVerticesGM (GraphMatrix gm) = Vertices [1 .. length gm]


{-|
    The 'numVerticesGM' function returns the number of vertices in a graph from its adjacency matrix representation.
    The function takes one argument, which is of type 'GraphMatrix'.

    For example:
		
    >>> gm
    1	2	5
    3	5	7
    0	0	4

	>>> numVerticesGM gm
	3
-}
numVerticesGM :: Num a => GraphMatrix a -> Int
numVerticesGM (GraphMatrix gm) = length gm


-- Find weight of edge between nodes i and j
weight :: GraphMatrix a -> Int -> Int -> a
weight (GraphMatrix gm) i j = (gm !! i) !! j


{-|
    The 'getEdgesGM' function returns the set of edges in a graph from its adjacency matrix representation.
    The function takes one argument, which is of type 'GraphMatrix'.

    For example:
		
    >>> gm
    1	2	5
    3	5	7
    0	0	4

	>>> getEdgesGM gm
	{(1,1,1),(1,2,2),(1,3,5),(2,1,3),(2,2,5),(2,3,7),(3,3,4)}
-}
getEdgesGM :: Integral a => GraphMatrix a -> Edges Int
getEdgesGM (GraphMatrix gm) = Edges [(i + 1, j + 1, w i j) | i <- [0 .. fromIntegral $ length (graphToMatrix (GraphMatrix gm)) - 1], j <- [0 .. fromIntegral (length (graphToMatrix (GraphMatrix gm)) - 1)], weight (GraphMatrix gm) i j /= 0]
  where
    w i j = fromIntegral $ weight (GraphMatrix gm) i j


{-|
    The 'numEdgesGM' function returns the number of edges in a graph from its adjacency matrix representation.
    The function takes one argument, which is of the type 'GraphMatrix'.

    For example:
		
    >>> gm
    1	2	5
    3	5	7
    0	0	4

	>>> numEdgesGM gm
	7
-}
numEdgesGM :: Integral a => GraphMatrix a -> Int
numEdgesGM (GraphMatrix gm) = length $ edgesToList $ getEdgesGM (GraphMatrix gm)


{-|
    The 'convertGM2G' function returns a Graph representation as G(V,E) from its adjacency matrix representation.
    The function takes one argument, which is of type 'GraphMatrix' and returns a 'Graph'.
    
    For example:
		
    >>> gm
    1	2	5
    3	5	7
    0	0	4

	>>> convertGM2G gm
	Graph ({1,2,3},{(1,1,1),(1,2,2),(1,3,5),(2,1,3),(2,2,5),(2,3,7),(3,3,4)})
-}
convertGM2G :: Integral a => GraphMatrix a -> Graph Int
convertGM2G (GraphMatrix gm) = Graph (getVerticesGM (GraphMatrix gm), getEdgesGM (GraphMatrix gm))


{-|
    The 'convertG2GM' function returns an adjacency matrix representation of a graph.
    The function takes one argument, which is of type 'Graph' and returns a 'GraphMatrix'.

    For example:
		
    >>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> convertG2GM g
	0	5	7	0
	0	0	0	3
	0	0	0	0
	0	0	0	0
-}
convertG2GM :: (Eq t, Num t) => Graph t -> GraphMatrix Int

convertG2GM' (Graph g) = [f i j | i <- verticesToList $ getVerticesG (Graph g), j <- verticesToList $ getVerticesG (Graph g)]
  where
    edgeList = [(first e, second e) | e <- edgesToList (getEdgesG (Graph g))]

    f i j = 
      if (i, j) `elem` edgeList
      then third (w i j (Graph g))
      else 0

    w i j (Graph g) = 
      head [ (i, j, k) | k <- [0 .. (maxWeight (Graph g))], (i, j, k) `elem` edgesToList (snd g) ]

    maxWeight (Graph g) = fromIntegral $ L.minimumBy (flip compare) [ third x | x <- edgesToList (snd g) ]


chunk' n = takeWhile (not . null) . map (take n) . iterate (drop n)


convertG2GM (Graph g) = GraphMatrix $ chunk' (numVerticesG (Graph g)) (convertG2GM' (Graph g))


{-|
    The 'transposeGM' function returns a transpose of a graph (in adjacency matrix form).
    The function takes one argument, which is of type 'GraphMatrix' and returns a 'GraphMatrix'.

    For example:

	>>> gm
	1	2	5
	3	5	7
	0	0	4

	>>> transposeGM gm
	1	3	0
	2	5	0
	5	7	4
-}
transposeGM :: Num a => GraphMatrix a -> GraphMatrix a
transposeGM (GraphMatrix []) = GraphMatrix []
transposeGM (GraphMatrix [[]]) = GraphMatrix [[]]
transposeGM (GraphMatrix xs) = GraphMatrix $ foldr (zipWith (:)) (repeat []) xs


{-|
    The 'transposeG' function returns a transpose of a graph.
    The function takes one argument, which is of type 'Graph' and returns another graph of type 'Graph'.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> transposeG g
	Graph ({1,2,3,4},{(2,1,5),(3,1,7),(4,2,3)})
-}
transposeG :: (Eq t, Num t) => Graph t -> Graph Int
transposeG (Graph g) = convertGM2G $ transposeGM $ convertG2GM (Graph g)


{-|
    The 'isUndirectedGM' function returns whether a graph (adjacency matrix form) is undirected or not.
    A graph is undirected if it is equal to its transpose.
    The function takes one argument, which is of type 'GraphMatrix'.

    Below are two examples:
		
    >>> gm1
    1	2	5
    3	5	7
    0	0	4

	>>> isUndirectedGM gm1
	False
	
	>>> gm2
	0	1	1
	1	1	0
	1	0	0

	>>> isUndirected gm2
	True
-}
isUndirectedGM :: (Num a, Eq a) => GraphMatrix a -> Bool
isUndirectedGM (GraphMatrix gm) = GraphMatrix gm == transposeGM (GraphMatrix gm)


{-|
    The 'isUndirectedG' function returns whether a graph is undirected or not.
    A graph is undirected if it is equal to its transpose.
    The function takes one argument, which is of type 'Graph'.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> isUndirectedG g
	False
-}
isUndirectedG :: (Num a, Eq a) => Graph a -> Bool
isUndirectedG (Graph g) = isUndirectedGM (convertG2GM (Graph g))


{-|
    The 'isDirectedGM' function returns whether a graph (adjacency matrix form) is directed or not.
    A graph is directed if it is not equal to its transpose.
    The function takes one argument, which is of type 'GraphMatrix'.

    Below are a few examples:
		
    >>> gm1
    1	3
    0	5

	>>> isDirected gm1
	True
	
	>>> gm2
	1	2	5
	3	5	7
	0	0	4

	>>> isDirectedGM gm2
	True
-}
isDirectedGM :: (Num a, Eq a) => GraphMatrix a -> Bool
isDirectedGM (GraphMatrix gm) = not $ isUndirectedGM (GraphMatrix gm)


{-|
    The 'isDirectedG' function returns whether a graph is directed or not.
    A graph is directed if it is not equal to its transpose.
    The function takes one argument, which is of type 'Graph'.

    Below are a few examples:

	>>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> isDirectedG g
	True
-}
isDirectedG :: (Eq t, Num t) => Graph t -> Bool
isDirectedG (Graph g) = isDirectedGM $ convertG2GM (Graph g)


{-|
    The 'unionG' function returns the union of two graphs.
    The function takes two arguments, both of the type 'Graph', and returns a 'Graph' type.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> g'
	Graph ({1,2,3},{(1,2,1),(1,4,5)})

	>>> unionG g g'
	Graph ({1,2,3,4},{(1,2,1),(1,2,5),(1,3,7),(1,4,5),(2,4,3)})
-}
unionG :: (Num a, Ord a) => Graph a -> Graph a -> Graph a
unionG (Graph g1) (Graph g2) = Graph (
  Vertices $ L.sort $ verticesToList (getVerticesG (Graph g1)) `L.union` verticesToList (getVerticesG (Graph g2)), 
  Edges $ L.sort $ edgesToList (getEdgesG (Graph g1)) `L.union` edgesToList (getEdgesG (Graph g2)))


{-|
    The 'unionGM' function returns the union of two graphs (in their adjancency matrix representation).
    The function takes two arguments, both of the type 'GraphMatrix', and returns a 'GraphMatrix' type,

    For example:

	>>> gm
	0	5	7	0
	0	0	0	3
	0	0	0	0
	0	0	0	0

	>>> gm'
	0	1	0
	0	0	0
	0	0	0

	>>> unionGM gm gm'
	0	1	7	0
	0	0	0	3
	0	0	0	0
	0	0	0	0
-}
unionGM :: (Integral a, Integral a1) => GraphMatrix a -> GraphMatrix a1 -> GraphMatrix Int
unionGM (GraphMatrix gm1) (GraphMatrix gm2) = convertG2GM (unionG (convertGM2G (GraphMatrix gm1)) (convertGM2G (GraphMatrix gm2)))


{-|
    The 'addVerticesG' function is used to add vertices to a graph.
    The function takes two arguments, one of type 'Graph' and the other of type 'Vertices'.
    The function adds the vertices and returns a 'Graph'.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

	>>> addVerticesG g (Vertices [5])
	Graph ({1,2,3,4,5},{(1,2,5),(1,3,7),(2,4,3)})

	>>> addVerticesG g (Vertices [5..8])
	Graph ({1,2,3,4,5,6,7,8},{(1,2,5),(1,3,7),(2,4,3)})
-}
addVerticesG :: (Num a, Eq a) => Graph a -> Vertices a -> Graph a
addVerticesG (Graph g) (Vertices v) = Graph (
  Vertices $ verticesToList (getVerticesG (Graph g)) `L.union` verticesToList (Vertices v),
  getEdgesG (Graph g))


{-|
    The 'addVerticesGM' function is used to add vertices to a graph (adjacency matrix representation.
    The function takes two arguments, one of type 'GraphMatrix' and the other of type 'Vertices'.
    The function adds the vertices and returns a 'GraphMatrix'.

    For example:

	>>> gm
	0	5	7	0
	0	0	0	3
	0	0	0	0
	0	0	0	0

	>>> addVerticesGM gm (Vertices [5])
	0	5	7	0	0
	0	0	0	3	0
	0	0	0	0	0
	0	0	0	0	0
	0	0	0	0	0

	>>> addVerticesGM gm (Vertices [5,6])
	0	5	7	0	0	0
	0	0	0	3	0	0
	0	0	0	0	0	0
	0	0	0	0	0	0
	0	0	0	0	0	0
	0	0	0	0	0	0
-}
addVerticesGM :: Integral a => GraphMatrix a -> Vertices Int -> GraphMatrix Int
addVerticesGM (GraphMatrix gm) (Vertices v) = convertG2GM $ addVerticesG (convertGM2G (GraphMatrix gm)) (Vertices v)


{-|
    The 'getVerticesFromEdges' function returns a list of vertices that form edges.
    The function takes one argument of type 'Edges' and retuns a list.

    For example:
		
    >>> e
    {(1,2,5),(1,3,7),(2,4,3)}

	>>> getVerticesFromEdges e
	[1,2,3,4]
-}
getVerticesFromEdges :: Eq a => Edges a -> [a]
getVerticesFromEdges (Edges e) = L.nub [first edge | edge <- edgesToList (Edges e)] `L.union` L.nub [second edge | edge <- edgesToList (Edges e)]


{-|
    The 'addEdgesG' function adds edges to a graph.
    The new edges must consist of the vertices already existing in the graph.
    The function takes two arguments, the first of type 'Graph' and the second of type 'Edges'.
    After adding the edges, the new 'Graph' is returned.

    For example:

	>>> g
	Graph ({1,2,3},{(1,2),(1,3)})

	>>> addEdgesG g (Edges [(2,3),(3,1)])
	Graph ({1,2,3},{(1,2),(1,3),(2,3),(3,1)})

	>>> addEdgesG g (Edges [(2,3),(3,1),(4,1)])
	Graph *** Exception: Vertices in the edge(s) are not in the graph's set of vertices.
-}
addEdgesG :: (Num a, Eq a) => Graph a -> Edges a -> Graph a
addEdgesG (Graph g) (Edges e) = 
  if and [v `elem` verticesToList (getVerticesG (Graph g)) | v <- getVerticesFromEdges (Edges e)]
  then Graph (
    getVerticesG (Graph g),
    Edges $ (edgesToList $ getEdgesG (Graph g)) `L.union` edgesToList (Edges e)
  )
  else error "Vertices in the edge(s) are not in the graph's set of vertices."


{-|
    The 'addEdgesGM' function adds edges to a graph (adjacency matrix representation).
    The new edges must consist of the vertices already existing in the graph.
    The function takes two arguments, the first of type 'GraphMatrix' and the second of type 'Edges'.
    After adding the edges, the new 'GraphMatrix' is returned.

    For example:

	>>> gm
	0	5	7	0
	0	0	0	3
	0	0	0	0
	0	0	0	0

	>>> addEdgesGM gm (Edges [(1,4,5),(2,3,6),(4,3,1)])
	0	5	7	5
	0	0	6	3
	0	0	0	0
	0	0	1	0
-}
addEdgesGM :: Integral a => GraphMatrix a -> Edges Int -> GraphMatrix Int
addEdgesGM (GraphMatrix gm) (Edges e) = convertG2GM $ addEdgesG (convertGM2G (GraphMatrix gm)) (Edges e)


----------Internal methods (matrix operations)-----------

-- Multiplication of two matrices
mMult' :: Num a => Matrix a -> Matrix a -> Matrix a
mMult' (Matrix m1) (Matrix m2) = Matrix [ map (multRow' r) m2t | r <- m1 ]
  where 
    (Matrix m2t) = mTranspose' (Matrix m2)
    multRow' r1 r2 = sum $ zipWith (*) r1 r2


-- Transpose of a Matrix
mTranspose' :: Matrix a -> Matrix a
mTranspose' (Matrix []) = Matrix []
mTranspose' (Matrix [[]]) = Matrix [[]]
mTranspose' xs = Matrix $ foldr (zipWith (:)) (repeat []) (mat2list' xs)


-- Converting a Matrix to list
mat2list' :: Matrix a -> [[a]]
mat2list' (Matrix m) = m


-- Inverting a Matrix
inverse' :: (Num a, Floating a) => Matrix a -> Matrix a
inverse' (Matrix m) = Matrix $ map (map (* recip (determinant' (Matrix m)))) $ mat2list' $ cofactorM' (Matrix m)


-- Calculating the determinant of a Matrix
determinant' :: (Num a, Floating a) => Matrix a -> a
determinant' (Matrix m)
  | numRows' (Matrix m) == 1 = head (head m)
  | otherwise    = sum $ zipWith addition [0..] m
  where
    addition i (x:_) =  x * cofactor' i 0 (Matrix m)


-- Calculating the cofactor of a Matrix
cofactor' :: (Num a, Floating a) => Int -> Int -> Matrix a -> a
cofactor' i j (Matrix m) = ((-1.0) ** fromIntegral (i + j)) * determinant' (delmatrix' i j (Matrix m))

cofactorM' :: (Num a, Floating a) => Matrix a -> Matrix a
cofactorM' (Matrix m) = Matrix $ map (map (\(i,j) -> cofactor' j i (Matrix m))) $ coords' (Matrix m)


-- Number of rows in a Matrix
numRows' :: Num a => Matrix a -> Int
numRows' (Matrix a) = length a


delmatrix' :: Num a => Int -> Int -> Matrix a -> Matrix a
delmatrix' i j (Matrix a) = Matrix $ dellist i $ map (dellist j) a
  where
    dellist i xs = take i xs ++ drop (i + 1) xs


coords' :: Num a => Matrix a -> [[(Int, Int)]]
coords' (Matrix a) = zipWith (map . (,)) [0..] $ map (zipWith const [0..]) a


-- Power of a matrix
mPower' :: (Num a, Floating a) => Matrix a -> Int -> Matrix a
mPower' (Matrix matrix) exp
  | exp < 0 = mPower' (inverse' (Matrix matrix)) (-exp)
  | exp == 0 = error "Exponent must be non-zero."
  | exp == 1 = Matrix matrix
  | otherwise = mMult' (Matrix matrix) (mPower' (Matrix matrix) (exp-1))
--------------------------------------------------


{-|
    The 'areConnectedGM' function is used to check if two vertices are directly (or even indirectly) connected.
    The function takes three arguments, a 'GraphMatrix' and two 'Vertices'.

    For example:

	>>> gm1
	0.0	1.0	1.0	0.0
	1.0	0.0	0.0	1.0
	1.0	0.0	0.0	1.0
	0.0	1.0	1.0	0.0

	>>> areConnectedGM gm1 (Vertices [1]) (Vertices [3])
	True
-}
areConnectedGM :: (Eq a, Floating a) => GraphMatrix a -> Vertices Int -> Vertices Int -> Bool
areConnectedGM (GraphMatrix g) (Vertices v1) (Vertices v2) =
  (mat2list' $ mPower' (Matrix $ graphToMatrix (GraphMatrix g)) (numVerticesGM (GraphMatrix g))) !! (head v1 - 1) !! (head v2 - 1) /= 0


{-|
    The 'numPathsBetweenGM' function is used to find the number of paths that exist between two given vertices in a graph.
    The function takes two arguments, the first is the 'GraphMatrix' and the other two are 'Vertices'.

    For example:

	>>> gm1
	0.0	1.0	1.0	0.0
	1.0	0.0	0.0	1.0
	1.0	0.0	0.0	1.0
	0.0	1.0	1.0	0.0

	>>> numPathsBetweenGM gm1 (Vertices [1]) (Vertices [4])
	8.0
-}
numPathsBetweenGM :: Floating a => GraphMatrix a -> Vertices Int -> Vertices Int -> a
numPathsBetweenGM (GraphMatrix g) (Vertices v1) (Vertices v2) =
  mat2list' (mPower' (Matrix $ graphToMatrix (GraphMatrix g)) (numVerticesGM (GraphMatrix g))) !! (head (verticesToList (Vertices v1)) - 1) !! (head (verticesToList (Vertices v2)) - 1)


{-|
    The 'adjacentNodesG' function returns all the adjacent vertices of a mentioned vertex.
    The function takes two arguments, the first is a 'Graph' and the second is a vertex of type 'Vertices'.

    For example:
		
    >>> g
    Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})
		
	>>> adjacentNodes g (Vertices [1])
	{2,3}

	>>> adjacentNodes g (Vertices [2])
	{1,4}

	>>> adjacentNodes g (Vertices [3])
	{1,4}

	>>> adjacentNodes g (Vertices [4])
	{2,3}
-}
adjacentNodesG :: (Num a, Eq a) => Graph a -> Vertices a -> Vertices a
adjacentNodesG (Graph g) (Vertices v) = Vertices $ L.union [ second x | x <- edgesToList $ getEdgesG (Graph g), first x == head v ] [ first y | y <- edgesToList $ getEdgesG (Graph g), second y == head v ]


{-|
    The 'adjacentNodesGM' function returns all the adjacent vertices of a mentioned vertex.
    The function takes two arguments, the first is a 'GraphMatrix' and the second is a vertex of type 'Vertices'.

    For example:
		
    >>> gm
    0	1	4	0
    5	0	0	1
    2	0	0	4
    0	1	1	0

	>>> adjacentNodesGM gm (Vertices [1])
	{2,3}

	>>> adjacentNodesGM gm (Vertices [2])
	{1,4}

	>>> adjacentNodesGM gm (Vertices [3])
	{1,4}

	>>> adjacentNodesGM gm (Vertices [4])
	{2,3}
-}
adjacentNodesGM :: Integral a => GraphMatrix a -> Vertices Int -> Vertices Int
adjacentNodesGM (GraphMatrix gm) (Vertices v) = adjacentNodesG (convertGM2G (GraphMatrix gm)) (Vertices v)


{-|
    The 'inDegreeG' function returns the in-degree of a vertex in a directed graph.
    The function takes two arguments, the first is a 'Graph' and the second is a vertex of type 'Vertices'

    For example:
		
    >>> g
    Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})

	>>> inDegreeG g (Vertices [1])
	2
-}
inDegreeG :: (Num a, Eq a) => Graph a -> Vertices a -> Int
inDegreeG (Graph g) (Vertices v) = length [ first y | y <- edgesToList $ getEdgesG (Graph g), second y == head v ]


{-|
    The 'inDegreeGM' function returns the in-degree of a vertex in a directed graph.
    The function takes two arguments, the first is a 'GraphMatrix' and the second is a vertex of type 'Vertices'

    For example:
		
    >>> gm
    0	1	4	0
    5	0	0	1
    2	0	0	4
    0	1	1	0

	>>> inDegreeGM gm (Vertices [2])
	2
-}
inDegreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
inDegreeGM (GraphMatrix gm) (Vertices v) = inDegreeG (convertGM2G (GraphMatrix gm)) (Vertices v)


{-|
    The 'outDegreeG' function returns the out-degree of a vertex in a directed graph.
    The function takes two arguments, the first is a 'Graph' and the second is a vertex of type 'Vertices'.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})
	
	>>> outDegree g (Vertices [4])
	2

	>>> outDegree g (Vertices [1])
	2
-}
outDegreeG :: (Eq a, Num a) => Graph a -> Vertices a -> Int
outDegreeG (Graph g) (Vertices v) = length [ second y | y <- edgesToList $ getEdgesG (Graph g), first y == head v ]


{-|
    The 'outDegreeGM' function returns the out-degree of a vertex in a directed graph.
    The function takes two arguments, the first is a 'GraphMatrix' and the second is a vertex of type 'Vertices'.

    For example:

	>>> gm
	0	1	4	0
	5	0	0	1
	2	0	0	4
	0	1	1	0

	>>> outDegreeGM gm (Vertices [3])
	2
-}
outDegreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
outDegreeGM (GraphMatrix gm) (Vertices v) = outDegreeG (convertGM2G (GraphMatrix gm)) (Vertices v)


{-|
    The 'degreeG' function returns the degree of a vertex in a undirected graph.
    The function takes two arguments, the first is the undirected 'Graph' and the second is a vertex of type 'Vertices'.

    For example:
		
    >>> g
    Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})

	>>> degreeG g (Vertices [4])
	4
-}
degreeG :: (Eq a, Num a) => Graph a -> Vertices a -> Int
degreeG (Graph g) (Vertices v)
  | isUndirectedG (Graph g) = inDegreeG (Graph g) (Vertices v) + outDegreeG (Graph g) (Vertices v)
  | otherwise = error "The graph you mentioned is not an undirected graph."


{-|
    The 'degreeGM' function returns the degree of a vertex in a undirected graph (adjacency matrix representation.
    The function takes two arguments, the first is the undirected 'GraphMatrix' and the second is a vertex of type 'Vertices'.

    For example:

	>>> gm
	0	1	4	0
	5	0	0	1
	2	0	0	4
	0	1	1	0

	>>> degreeGM gm (Vertices [2])
	4
-}
degreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
degreeGM (GraphMatrix gm) (Vertices v)
  | isUndirectedGM (GraphMatrix gm) = inDegreeGM (GraphMatrix gm) (Vertices v) + outDegreeGM (GraphMatrix gm) (Vertices v)
  | otherwise = error "The graph you mentioned is not an undirected graph."


{-|
    The 'hasEulerCircuitG' function checks if a specified graph conrtains a Euler Circuit.

    For example:
		
    >>> g
    Graph ({1,2,3,4,5},{(1,2,1),(1,5,1),(2,1,1),(2,5,1),(3,4,1),(3,5,1),(4,3,1),(4,5,1),(5,1,1),(5,2,1),(5,3,1),(5,4,1)})

	>>> hasEulerCircuitG g
	True
-}
hasEulerCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasEulerCircuitG (Graph g) = and [ even $ degreeG (Graph g) (Vertices [v]) | v <- verticesToList $ getVerticesG (Graph g)]


{-|
    The 'hasEulerCircuitGM' function checks if a specified graph (adjacency matrix representation) conrtains a Euler Circuit.

    For example:
		
    >>> gm
    0	1	0	0	1
    1	0	0	0	1
    0	0	0	1	1
    0	0	1	0	1
    1	1	1	1	0

	>>> hasEulerCircuitGM gm
	True
-}
hasEulerCircuitGM :: Integral a => GraphMatrix a -> Bool
hasEulerCircuitGM (GraphMatrix gm) = hasEulerCircuitG (convertGM2G (GraphMatrix gm))


{-|
    The 'hasEulerPathG' function checks if a graph contains a Euler Path.

    For example:

	>>> g
	Graph ({1,2,3,4},{(1,2,1),(1,4,1),(2,1,1),(2,3,1),(2,4,1),(3,2,1),(3,4,1),(4,1,1),(4,2,1),(4,3,1)})

	>>> hasEulerPathG g
	True
-}
hasEulerPathG :: (Eq a, Num a) => Graph a -> Bool
hasEulerPathG (Graph g) = hasEulerCircuitG (Graph g)


{-|
    The 'hasEulerPathGM' function checks if a graph (adjacency matrix representation) contains a Euler Path.

    For example:

	>>> gm
	0	1	0	1
	1	0	1	1
	0	1	0	1
	1	1	1	0

	>>> hasEulerPathGM gm
	True
-}
hasEulerPathGM :: Integral a => GraphMatrix a -> Bool
hasEulerPathGM (GraphMatrix gm) = hasEulerCircuitGM (GraphMatrix gm)


{-|
    The 'countOddDegreeV' function returns the number of vertices in a graph that have an odd degree.

    For example:
		
    >>> g
    Graph ({1,2,3,4},{(1,2,1),(1,4,1),(2,1,1),(2,3,1),(2,4,1),(3,2,1),(3,4,1),(4,1,1),(4,2,1),(4,3,1)})

	>>> countOddDegreeV g
	0
	
	>>> g1
	Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

	>>> countOddDegreeV g1
	2
-}
countOddDegreeV :: (Eq a1, Num a, Num a1) => Graph a1 -> a
countOddDegreeV (Graph g) = sum [ 1 | v <- verticesToList $ getVerticesG (Graph g), odd $ degreeG (Graph g) (Vertices [v]) ]


{-|
    The 'countEvenDegreeV' function returns the number of vertices in a graph that have an even degree.

    For example:

	>>> g1
	Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

	>>> countEvenDegreeV g1
	2
-}
countEvenDegreeV :: (Eq a1, Num a, Num a1) => Graph a1 -> a
countEvenDegreeV (Graph g) = sum [ 1 | v <- verticesToList $ getVerticesG (Graph g), even $ degreeG (Graph g) (Vertices [v]) ]


{-|
    The 'hasEulerPathNotCircuitG' function checks if a graph conatains a Euler Path but not a Euler circuit.

    For example:

	>>> g1
	Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

	>>> hasEulerPathNotCircuitG g1
	True
-}
hasEulerPathNotCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasEulerPathNotCircuitG (Graph g) = countOddDegreeV (Graph g) == 2


{-|
    The 'hasEulerPathNotCircuitGM' function checks if a graph (adjacency matrix representation) conatains a Euler Path but not a Euler circuit.

    For example:

	>>> gm
	0	4	1	5
	0	0	0	0
	0	6	0	0
	0	0	0	0

	>>> hasEulerPathNotCircuitGM gm
	True
-}
hasEulerPathNotCircuitGM :: Integral a => GraphMatrix a -> Bool
hasEulerPathNotCircuitGM (GraphMatrix gm) = hasEulerPathNotCircuitG (convertGM2G (GraphMatrix gm))


{-|
    The 'hasHamiltonianCircuitG' function checks if a graph contains a Hamiltonian Circuit.

    For example:

	>>> g2
	Graph ({1,2,3,4,5},{(1,2,1),(1,3,1),(1,5,1),(2,1,1),(2,3,1),(2,5,1),(3,1,1),(3,2,1),(3,4,1),(3,5,1),(4,3,1),(4,5,1),(5,1,1),(5,2,1),(5,3,1),(5,4,1)})

	>>> hasHamiltonianCircuitG g2
	True
-}
hasHamiltonianCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasHamiltonianCircuitG (Graph g) = and [degreeG (Graph g) (Vertices [v]) >= (numVerticesG (Graph g) `div` 2) | v <- verticesToList $ getVerticesG (Graph g), numVerticesG (Graph g) >= 3]


{-|
    The 'hasHamiltonianCircuitGM' function checks if a graph (adjacency matrix representation) contains a Hamiltonian Circuit.

    For example:
		
    >>> gm
    0	1	1	0	1
    1	0	1	0	1
    1	1	0	1	1
    0	0	1	0	1
    1	1	1	1	0

	>>> hasHamiltonianCircuitGM gm
	True
-}
hasHamiltonianCircuitGM :: Integral a => GraphMatrix a -> Bool
hasHamiltonianCircuitGM (GraphMatrix gm) = hasHamiltonianCircuitG (convertGM2G (GraphMatrix gm))


{-|
    The 'isSubgraphG' function checks if a graph is a sub-graph if another specified graph.

    For example:

	>>> g1
	Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

	>>> g2
	Graph ({1,3},{(1,3,1)})

	>>> isSubgraphG g2 g1
	True

	>>> isSubgraphG g1 g2
	False
-}
isSubgraphG :: (Num a, Ord a) => Graph a -> Graph a -> Bool
isSubgraphG (Graph g1) (Graph g2) = (e1 `isSubset` e2) && (v1 `isSubset` v2)
  where
    isSubset set1 set2 = null [e | e <- (L.sort . L.nub) set1, e `notElem` (L.sort . L.nub) set2]
    e1 = edgesToList $ getEdgesG (Graph g1)
    e2 = edgesToList $ getEdgesG (Graph g2)
    v1 = verticesToList $ getVerticesG (Graph g1)
    v2 = verticesToList $ getVerticesG (Graph g2)


{-|
    The 'isSubgraphGM' function checks if a graph is a sub-graph if another specified graph (both graphs as 'GraphMatrix' - adjacency matrix representation).

    For example:
		
    >>> grm1
    0	4	1	5
    0	0	0	0
    0	6	0	0
    0	0	0	0

	>>> grm2
	0	4
	0	0

	>>> isSubgraphGM grm2 grm1
	True

	>>> isSubgraphGM grm1 grm2
	False
-}
isSubgraphGM :: (Integral a, Integral a1) => GraphMatrix a -> GraphMatrix a1 -> Bool
isSubgraphGM (GraphMatrix gm1) (GraphMatrix gm2) = isSubgraphG (convertGM2G (GraphMatrix gm1)) (convertGM2G (GraphMatrix gm2))


-- SAMPLE GRAPHS --
v1 = Vertices [1,2,3,4]
v2 = Vertices [1,2,3]
e1 = Edges [(1,2,4),(1,3,1),(1,4,5),(3,2,6)]
e2 = Edges [(1,2,1),(1,1,2),(3,1,3)]
g1 = Graph (v1,e1)
g2 = Graph (v2,e2)

gm1 = GraphMatrix [[0,1,3,5],[4,0,2,6],[1,2,3,4],[0,0,1,6]]

gm2 = GraphMatrix [[0,1,2],[1,0,3],[2,3,0]]
