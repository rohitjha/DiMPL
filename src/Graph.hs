{-
-------------------------
| MPL Module for Graphs |
-------------------------

Functionality for
	-> Data types
		-Graph(V,E)
		-GraphMatrix
		-Edges
		-Vertices
	-> Adding edges and vertices
	-> Getting vertices and their number
	-> Getting edges and their number
	-> Converting between different Graph data types
	-> Transpose
	-> Checking for directedness
	-> Union
	-> Connectedness of vertices
	-> Number of paths between two vertices
	-> Adjacent nodes of a vertex
	-> Degrees of vertices
	-> Euler circuits and paths
	-> Hamiltonian circuits

Author: Rohit Jha, Alfy Samuel, Ashmee Pawar
Version: 0.1
Date: 31 Jan 2013
-}

module Graph
(
  Vertices(..),
  vertices2list,
  Edges(..),
  edges2list,
  first,
  second,
  third,
  Graph(..),
  GraphMatrix(..),
  graph2matrix,
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
  gTransposeG,
  gTransposeGM,
  isUndirectedG,
  isUndirectedGM,
  isDirectedG,
  isDirectedGM,
  unionG,
  unionGM,
  addVerticesG,
  addVerticesGM,
  verticesInEdges,
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


-- Data type for vertices
{-
	Usage:
		Vertices []
		>>> {}
	
		Vertices [1,2,3]
		>>> {1,2,3}

		Vertices [1..10]
		>>> {1,2,3,4,5,6,7,8,9,10}
-}
newtype Vertices a = Vertices [a] deriving (Eq)

instance (Show a) => Show (Vertices a) where
  showsPrec _ (Vertices s) = showVertices s

showVertices [] str = showString "{}" str
showVertices (x:xs) str = showChar '{' (shows x (showl xs str))
  where 
    showl [] str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

vertices2list (Vertices v) = v


-- Data types for edges
{-
	Usage:
		Edges [(1,1,10)]
		>>> {(1,1,10)}

		Edges [(1,2,1),(1,3,5),(2,3,2)]
		>>> {(1,2,1),(1,3,5),(2,3,2)}

		Edges []
		>>> {}
-}
newtype Edges a = Edges [(a,a,Int)] deriving (Eq)

instance (Show a) => Show (Edges a) where
  showsPrec _ (Edges s) = showEdges s

showEdges [] str = showString "{}" str
showEdges (x:xs) str = showChar '{' (shows x (showl xs str))
  where 
    showl [] str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

edges2list (Edges a) = a

first (a,b,c) = a
second (a,b,c) = b
third (a,b,c) = c


-- Data type for Graph
{-
	Usage:
		let v = Vertices [1,2,3,4]
		let e = Edges [(1,2,5),(1,3,7),(2,4,3)]
		let g = Graph (v,e)
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})
-}
newtype Graph a = Graph (Vertices a, Edges a) deriving (Eq, Show)


-- Data type for Graph as matrix
{-
	Usage:
		GraphMatrix [[1,2,5],[3,5,7],[0,0,4]]
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4
-}
newtype Matrix a = Matrix [[a]] deriving (Eq)

instance Show a => Show (Matrix a) where
  show (Matrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


newtype GraphMatrix a = GraphMatrix [[a]] deriving (Eq)

instance Show a => Show (GraphMatrix a) where
  show (GraphMatrix a) = L.intercalate "\n" $ map (L.intercalate "\t" . map show) a


graph2matrix :: GraphMatrix a -> [[a]]
graph2matrix (GraphMatrix gm) = gm


-- Get vertices of a Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		getVerticesG g
		>>> {1,2,3,4}
-}
getVerticesG :: Num a => Graph a -> Vertices a
getVerticesG (Graph g) = fst g


-- Number of vertices of Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		numVerticesG g
		>>> 4
-}
numVerticesG :: Num a => Graph a -> Int
numVerticesG (Graph g) = length $ vertices2list $ getVerticesG (Graph g)


-- Number of edges of a Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		numEdgesG g
		>>> 3
-}
numEdgesG :: Num a => Graph a -> Int
numEdgesG (Graph g) = length $ edges2list $ getEdgesG (Graph g)


-- Get edges of a Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		getEdgesG g
		>>> {(1,2,5),(1,3,7),(2,4,3)}
-}
getEdgesG :: Num a => Graph a -> Edges a
getEdgesG (Graph g) = snd g


-- Get vertices of a GraphMatrix
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		getVerticesGM gm
		>>> {1,2,3}
-}
getVerticesGM :: GraphMatrix t -> Vertices Int
getVerticesGM (GraphMatrix gm) = Vertices [1 .. length gm]


-- Get number of vertices of GraphMatrix
{-
	Usage:	
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		numVerticesGM gm
		>>> 3
-}
numVerticesGM :: Num a => GraphMatrix a -> Int
numVerticesGM (GraphMatrix gm) = length gm


-- Find weight of edge between nodes i and j
weight :: GraphMatrix a -> Int -> Int -> a
weight (GraphMatrix gm) i j = (gm !! i) !! j


-- Get edges of a GraphMatrix
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		getEdgesGM gm
		>>> {(1,1,1),(1,2,2),(1,3,5),(2,1,3),(2,2,5),(2,3,7),(3,3,4)}
-}
getEdgesGM :: Integral a => GraphMatrix a -> Edges Int
getEdgesGM (GraphMatrix gm) = Edges [(i + 1, j + 1, w i j) | i <- [0 .. fromIntegral $ length (graph2matrix (GraphMatrix gm)) - 1], j <- [0 .. fromIntegral (length (graph2matrix (GraphMatrix gm)) - 1)], weight (GraphMatrix gm) i j /= 0]
  where
    w i j = fromIntegral $ weight (GraphMatrix gm) i j


-- Number of edges in a GraphMatrix
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		numEdgesGM gm
		>>> 7
-}
numEdgesGM :: Integral a => GraphMatrix a -> Int
numEdgesGM (GraphMatrix gm) = length $ edges2list $ getEdgesGM (GraphMatrix gm)


-- Convert GraphMatrix to Graph
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		convertGM2G gm
		>>> Graph ({1,2,3},{(1,1,1),(1,2,2),(1,3,5),(2,1,3),(2,2,5),(2,3,7),(3,3,4)})
-}
convertGM2G :: Integral a => GraphMatrix a -> Graph Int
convertGM2G (GraphMatrix gm) = Graph (getVerticesGM (GraphMatrix gm), getEdgesGM (GraphMatrix gm))



-- Convert Graph to adjacency GraphMatrix
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		convertG2GM g
		>>>	0	5	7	0
		>>>	0	0	0	3
		>>>	0	0	0	0
		>>>	0	0	0	0
-}
convertG2GM :: (Eq t, Num t) => Graph t -> GraphMatrix Int

convertG2GM' (Graph g) = [f i j | i <- vertices2list $ getVerticesG (Graph g), j <- vertices2list $ getVerticesG (Graph g)]
  where
    edgeList = [(first e, second e) | e <- edges2list (getEdgesG (Graph g))]

    f i j = 
      if (i, j) `elem` edgeList
      then third (w i j (Graph g))
      else 0

    w i j (Graph g) = 
      head [ (i, j, k) | k <- [0 .. (maxWeight (Graph g))], (i, j, k) `elem` edges2list (snd g) ]

    maxWeight (Graph g) = fromIntegral $ L.minimumBy (flip compare) [ third x | x <- edges2list (snd g) ]


chunk' n = takeWhile (not . null) . map (take n) . iterate (drop n)


convertG2GM (Graph g) = GraphMatrix $ chunk' (numVerticesG (Graph g)) (convertG2GM' (Graph g))


-- Transpose of a graph (GraphMatrix)
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		gTransposeGM gm
		>>>	1	3	0
		>>>	2	5	0
		>>>	5	7	4
-}
gTransposeGM :: Num a => GraphMatrix a -> GraphMatrix a
gTransposeGM (GraphMatrix []) = GraphMatrix []
gTransposeGM (GraphMatrix [[]]) = GraphMatrix [[]]
gTransposeGM (GraphMatrix xs) = GraphMatrix $ foldr (zipWith (:)) (repeat []) xs


-- Transpose of a graph (Graph)
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		gTransposeG g
		>>> Graph ({1,2,3,4},{(2,1,5),(3,1,7),(4,2,3)})
-}
gTransposeG :: (Eq t, Num t) => Graph t -> Graph Int
gTransposeG (Graph g) = convertGM2G $ gTransposeGM $ convertG2GM (Graph g)


-- Checking if a GraphMatrix is undirected
{-
	Usage:
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		isUndirectedGM gm
		>>> False
		
		
		gm
		>>>	0	1	1
		>>>	1	1	0
		>>>	1	0	0

		isUndirected gm
		>>> True
-}
isUndirectedGM :: (Num a, Eq a) => GraphMatrix a -> Bool
isUndirectedGM (GraphMatrix gm) = GraphMatrix gm == gTransposeGM (GraphMatrix gm)


-- Checking if a Graph is undirected
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		isUndirectedG g
		>>> False
-}
isUndirectedG :: (Num a, Eq a) => Graph a -> Bool
isUndirectedG (Graph g) = isUndirectedGM (convertG2GM (Graph g))


-- Checking if a GraphMatrix is directed
{-
	Usage:
		gm
		>>>	1	3
		>>>	0	5

		isDirected gm
		>>> True
		
		
		gm
		>>>	1	2	5
		>>>	3	5	7
		>>>	0	0	4

		isDirectedGM gm
		>>> True
-}
isDirectedGM :: (Num a, Eq a) => GraphMatrix a -> Bool
isDirectedGM (GraphMatrix gm) = not $ isUndirectedGM (GraphMatrix gm)


-- Checking if a Graph is directed
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		isDirectedG g
		>>> True
-}
isDirectedG :: (Eq t, Num t) => Graph t -> Bool
isDirectedG (Graph g) = isDirectedGM $ convertG2GM (Graph g)


-- Union of Graphs
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		g'
		>>> Graph ({1,2,3},{(1,2,1),(1,4,5)})

		unionG g g'
		>>> Graph ({1,2,3,4},{(1,2,1),(1,2,5),(1,3,7),(1,4,5),(2,4,3)})
-}
unionG :: (Num a, Ord a) => Graph a -> Graph a -> Graph a
unionG (Graph g1) (Graph g2) = Graph (
  Vertices $ L.sort $ vertices2list (getVerticesG (Graph g1)) `L.union` vertices2list (getVerticesG (Graph g2)), 
  Edges $ L.sort $ edges2list (getEdgesG (Graph g1)) `L.union` edges2list (getEdgesG (Graph g2)))


-- Union of GraphMatrices
{-
	Usage:
		gm
		>>>	0	5	7	0
		>>>	0	0	0	3
		>>>	0	0	0	0
		>>>	0	0	0	0

		gm'
		>>>	0	1	0
		>>>	0	0	0
		>>>	0	0	0

		unionGM gm gm'
		>>>	0	1	7	0
		>>>	0	0	0	3
		>>>	0	0	0	0
		>>>	0	0	0	0
-}
unionGM :: (Integral a, Integral a1) => GraphMatrix a -> GraphMatrix a1 -> GraphMatrix Int
unionGM (GraphMatrix gm1) (GraphMatrix gm2) = convertG2GM (unionG (convertGM2G (GraphMatrix gm1)) (convertGM2G (GraphMatrix gm2)))


-- Adding vertices to Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,5),(1,3,7),(2,4,3)})

		addVerticesG g (Vertices [5])
		>>> Graph ({1,2,3,4,5},{(1,2,5),(1,3,7),(2,4,3)})

		addVerticesG g (Vertices [5..8])
		>>> Graph ({1,2,3,4,5,6,7,8},{(1,2,5),(1,3,7),(2,4,3)})
-}
addVerticesG :: (Num a, Eq a) => Graph a -> Vertices a -> Graph a
addVerticesG (Graph g) (Vertices v) = Graph (
  Vertices $ vertices2list (getVerticesG (Graph g)) `L.union` vertices2list (Vertices v),
  getEdgesG (Graph g))


-- Adding vertices to GraphMatrix
{-
	Usage:
		gm
		>>>	0	5	7	0
		>>>	0	0	0	3
		>>>	0	0	0	0
		>>>	0	0	0	0

		addVerticesGM gm (Vertices [5])
		>>>	0	5	7	0	0
		>>>	0	0	0	3	0
		>>>	0	0	0	0	0
		>>>	0	0	0	0	0
		>>>	0	0	0	0	0

		addVerticesGM gm (Vertices [5,6])
		>>>	0	5	7	0	0	0
		>>>	0	0	0	3	0	0
		>>>	0	0	0	0	0	0
		>>>	0	0	0	0	0	0
		>>>	0	0	0	0	0	0
		>>>	0	0	0	0	0	0
-}
addVerticesGM :: Integral a => GraphMatrix a -> Vertices Int -> GraphMatrix Int
addVerticesGM (GraphMatrix gm) (Vertices v) = convertG2GM $ addVerticesG (convertGM2G (GraphMatrix gm)) (Vertices v)


-- Extracting all vertices in Edges
{-
	Usage:
		e
		>>> {(1,2,5),(1,3,7),(2,4,3)}

		verticesInEdges e
		>>> [1,2,3,4]
-}
verticesInEdges :: Eq a => Edges a -> [a]
verticesInEdges (Edges e) = L.nub [first edge | edge <- edges2list (Edges e)] `L.union` L.nub [second edge | edge <- edges2list (Edges e)]


-- Adding edges to Graph
{-
	Usage:
		 g
		>>> Graph ({1,2,3},{(1,2),(1,3)})

		addEdgesG g (Edges [(2,3),(3,1)])
		>>> Graph ({1,2,3},{(1,2),(1,3),(2,3),(3,1)})

		addEdgesG g (Edges [(2,3),(3,1),(4,1)])
		>>> Graph *** Exception: Vertices in the edge(s) are not in the graph's set of vertices.
-}
addEdgesG :: (Num a, Eq a) => Graph a -> Edges a -> Graph a
addEdgesG (Graph g) (Edges e) = 
  if and [v `elem` vertices2list (getVerticesG (Graph g)) | v <- verticesInEdges (Edges e)]
  then Graph (
    getVerticesG (Graph g),
    Edges $ (edges2list $ getEdgesG (Graph g)) `L.union` edges2list (Edges e)
  )
  else error "Vertices in the edge(s) are not in the graph's set of vertices."


-- Adding edges to GraphMatrix
{-
	Usage:
		gm
		>>>	0	5	7	0
		>>>	0	0	0	3
		>>>	0	0	0	0
		>>>	0	0	0	0

		addEdgesGM gm (Edges [(1,4,5),(2,3,6),(4,3,1)])
		>>>	0	5	7	5
		>>>	0	0	6	3
		>>>	0	0	0	0
		>>>	0	0	1	0
-}
addEdgesGM :: Integral a => GraphMatrix a -> Edges Int -> GraphMatrix Int
addEdgesGM (GraphMatrix gm) (Edges e) = convertG2GM $ addEdgesG (convertGM2G (GraphMatrix gm)) (Edges e)


----------Internal methods (matrix operations)-----------
mMult' :: Num a => Matrix a -> Matrix a -> Matrix a
mMult' (Matrix m1) (Matrix m2) = Matrix [ map (multRow' r) m2t | r <- m1 ]
  where 
    (Matrix m2t) = mTranspose' (Matrix m2)
    multRow' r1 r2 = sum $ zipWith (*) r1 r2


mTranspose' :: Matrix a -> Matrix a
mTranspose' (Matrix []) = Matrix []
mTranspose' (Matrix [[]]) = Matrix [[]]
mTranspose' xs = Matrix $ foldr (zipWith (:)) (repeat []) (mat2list' xs)


mat2list' :: Matrix a -> [[a]]
mat2list' (Matrix m) = m


inverse' :: (Num a, Floating a) => Matrix a -> Matrix a
inverse' (Matrix m) = Matrix $ map (map (* recip (determinant' (Matrix m)))) $ mat2list' $ cofactorM' (Matrix m)


determinant' :: (Num a, Floating a) => Matrix a -> a
determinant' (Matrix m)
  | numRows' (Matrix m) == 1 = head (head m)
  | otherwise    = sum $ zipWith addition [0..] m
  where
    addition i (x:_) =  x * cofactor' i 0 (Matrix m)


cofactor' :: (Num a, Floating a) => Int -> Int -> Matrix a -> a
cofactor' i j (Matrix m) = ((-1.0) ** fromIntegral (i + j)) * determinant' (delmatrix' i j (Matrix m))


cofactorM' :: (Num a, Floating a) => Matrix a -> Matrix a
cofactorM' (Matrix m) = Matrix $ map (map (\(i,j) -> cofactor' j i (Matrix m))) $ coords' (Matrix m)


numRows' :: Num a => Matrix a -> Int
numRows' (Matrix a) = length a


delmatrix' :: Num a => Int -> Int -> Matrix a -> Matrix a
delmatrix' i j (Matrix a) = Matrix $ dellist i $ map (dellist j) a
  where
    dellist i xs = take i xs ++ drop (i + 1) xs


coords' :: Num a => Matrix a -> [[(Int, Int)]]
coords' (Matrix a) = zipWith (map . (,)) [0..] $ map (zipWith const [0..]) a


mPower' :: (Num a, Floating a) => Matrix a -> Int -> Matrix a
mPower' (Matrix matrix) exp
  | exp < 0 = mPower' (inverse' (Matrix matrix)) (-exp)
  | exp == 0 = error "Exponent must be non-zero."
  | exp == 1 = Matrix matrix
  | otherwise = mMult' (Matrix matrix) (mPower' (Matrix matrix) (exp-1))
--------------------------------------------------


-- Checking if two vertices in GraphMatrix are connected
{-
	Usage:
		gm1
		>>>	0.0	1.0	1.0	0.0
			1.0	0.0	0.0	1.0
			1.0	0.0	0.0	1.0
			0.0	1.0	1.0	0.0

		areConnectedGM gm1 (Vertices [1]) (Vertices [3])
		>>> True
-}
areConnectedGM :: (Eq a, Floating a) => GraphMatrix a -> Vertices Int -> Vertices Int -> Bool
areConnectedGM (GraphMatrix g) (Vertices v1) (Vertices v2) =
  (mat2list' $ mPower' (Matrix $ graph2matrix (GraphMatrix g)) (numVerticesGM (GraphMatrix g))) !! (head v1 - 1) !! (head v2 - 1) /= 0


-- Checking if two vertices in Graph are connected
--areConnectedG (Graph g) (Vertices v1) (Vertices v2) = areConnectedGM (convertG2GM (Graph g)) (Vertices v1) (Vertices v2)


-- Finding number of paths between two vertices in a GraphMatrix
{-
	Usage:	
		gm1
		>>>	0.0	1.0	1.0	0.0
		>>>	1.0	0.0	0.0	1.0
		>>>	1.0	0.0	0.0	1.0
		>>>	0.0	1.0	1.0	0.0

		numPathsBetweenGM gm1 (Vertices [1]) (Vertices [4])
		>>> 8.0
-}
numPathsBetweenGM :: Floating a => GraphMatrix a -> Vertices Int -> Vertices Int -> a
numPathsBetweenGM (GraphMatrix g) (Vertices v1) (Vertices v2) =
  mat2list' (mPower' (Matrix $ graph2matrix (GraphMatrix g)) (numVerticesGM (GraphMatrix g))) !! (head (vertices2list (Vertices v1)) - 1) !! (head (vertices2list (Vertices v2)) - 1)


-- Finding number of paths between two vertices in a Graph
--numPathsBetweenG (Graph g) (Vertices v1) (Vertices v2) = numPathsBetweenGM (convertG2GM (Graph g)) (Vertices v1) (Vertices v2)


-- Finding nodes adjacent to a node in a Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})
		
		adjacentNodes g (Vertices [1])
		>>> {2,3}

		adjacentNodes g (Vertices [2])
		>>> {1,4}

		adjacentNodes g (Vertices [3])
		>>> {1,4}

		adjacentNodes g (Vertices [4])
		>>> {2,3}
-}
adjacentNodesG :: (Num a, Eq a) => Graph a -> Vertices a -> Vertices a
adjacentNodesG (Graph g) (Vertices v) = Vertices $ L.union [ second x | x <- edges2list $ getEdgesG (Graph g), first x == head v ] [ first y | y <- edges2list $ getEdgesG (Graph g), second y == head v ]


-- Finding nodes adjacent to a node in a GraphMatrix
{-
	Usage:
		gm
		>>>	0	1	4	0
			5	0	0	1
			2	0	0	4
			0	1	1	0

		adjacentNodesGM gm (Vertices [1])
		>>> {2,3}

		adjacentNodesGM gm (Vertices [2])
		>>> {1,4}

		adjacentNodesGM gm (Vertices [3])
		>>> {1,4}

		adjacentNodesGM gm (Vertices [4])
		>>> {2,3}
-}
adjacentNodesGM :: Integral a => GraphMatrix a -> Vertices Int -> Vertices Int
adjacentNodesGM (GraphMatrix gm) (Vertices v) = adjacentNodesG (convertGM2G (GraphMatrix gm)) (Vertices v)


-- In-degree of a vertex in a directed Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})

		inDegreeG g (Vertices [1])
		>>> 2
-}
inDegreeG :: (Num a, Eq a) => Graph a -> Vertices a -> Int
inDegreeG (Graph g) (Vertices v) = length [ first y | y <- edges2list $ getEdgesG (Graph g), second y == head v ]


-- In-degree of a vertex in a directed GraphMatrix
{-
	Usage:
		gm
		>>>	0	1	4	0
			5	0	0	1
			2	0	0	4
			0	1	1	0

		inDegreeGM gm (Vertices [2])
		>>> 2
-}
inDegreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
inDegreeGM (GraphMatrix gm) (Vertices v) = inDegreeG (convertGM2G (GraphMatrix gm)) (Vertices v)


-- Out-degree of a vertex in a directed Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})
		
		outDegree g (Vertices [4])
		>>> 2

		outDegree g (Vertices [1])
		>>> 2
-}
outDegreeG :: (Eq a, Num a) => Graph a -> Vertices a -> Int
outDegreeG (Graph g) (Vertices v) = length [ second y | y <- edges2list $ getEdgesG (Graph g), first y == head v ]


-- Out-degree of a vertex in a directed GraphMatrix
{-
	Usage:
		gm
		>>>	0	1	4	0
			5	0	0	1
			2	0	0	4
			0	1	1	0

		outDegreeGM gm (Vertices [3])
		>>> 2
-}
outDegreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
outDegreeGM (GraphMatrix gm) (Vertices v) = outDegreeG (convertGM2G (GraphMatrix gm)) (Vertices v)


-- Degree of a vertex in an undirected Graph
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,3,4),(2,1,5),(2,4,1),(3,1,2),(3,4,4),(4,2,1),(4,3,1)})

		degreeG g (Vertices [4])
		>>> 4
-}
degreeG :: (Eq a, Num a) => Graph a -> Vertices a -> Int
degreeG (Graph g) (Vertices v) = inDegreeG (Graph g) (Vertices v) + outDegreeG (Graph g) (Vertices v)


-- Degree of a vertex in an undirected GraphMatrix
{-
	Usage:
		gm
		>>>	0	1	4	0
			5	0	0	1
			2	0	0	4
			0	1	1	0

		degreeGM gm (Vertices [2])
		>>> 4
-}
degreeGM :: Integral a => GraphMatrix a -> Vertices Int -> Int
degreeGM (GraphMatrix gm) (Vertices v) = inDegreeGM (GraphMatrix gm) (Vertices v) + outDegreeGM (GraphMatrix gm) (Vertices v)


-- Finding if a Graph contains a Euler Circuit
{-
	Usage:
		g
		>>> Graph ({1,2,3,4,5},{(1,2,1),(1,5,1),(2,1,1),(2,5,1),(3,4,1),(3,5,1),(4,3,1),(4,5,1),(5,1,1),(5,2,1),(5,3,1),(5,4,1)})

		hasEulerCircuitG g
		>>> True
-}
hasEulerCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasEulerCircuitG (Graph g) = and [ even $ degreeG (Graph g) (Vertices [v]) | v <- vertices2list $ getVerticesG (Graph g)]


-- Finding if a GraphMatrix contains a Euler Circuit
{-
	Usage:
		gm
		>>>	0	1	0	0	1
			1	0	0	0	1
			0	0	0	1	1
			0	0	1	0	1
			1	1	1	1	0

		hasEulerCircuitGM gm
		>>> True
-}
hasEulerCircuitGM :: Integral a => GraphMatrix a -> Bool
hasEulerCircuitGM (GraphMatrix gm) = hasEulerCircuitG (convertGM2G (GraphMatrix gm))


-- Finding if a Graph contains a Euler Path
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,4,1),(2,1,1),(2,3,1),(2,4,1),(3,2,1),(3,4,1),(4,1,1),(4,2,1),(4,3,1)})

		hasEulerPathG g
		>>> True
-}
hasEulerPathG :: (Eq a, Num a) => Graph a -> Bool
hasEulerPathG (Graph g) = hasEulerCircuitG (Graph g)


-- Finding if a GraphMatrix contains a Euler Path
{-
	Usage:
		gm
		>>>	0	1	0	1
			1	0	1	1
			0	1	0	1
			1	1	1	0

		hasEulerPathGM gm
		>>> True
-}
hasEulerPathGM :: Integral a => GraphMatrix a -> Bool
hasEulerPathGM (GraphMatrix gm) = hasEulerCircuitGM (GraphMatrix gm)


-- Finding number of vertices with odd degree
{-
	Usage:
		g
		>>> Graph ({1,2,3,4},{(1,2,1),(1,4,1),(2,1,1),(2,3,1),(2,4,1),(3,2,1),(3,4,1),(4,1,1),(4,2,1),(4,3,1)})

		countOddDegreeV g
		>>> 0
		
		g1
		>>> Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

		countOddDegreeV g1
		>>> 2
-}
countOddDegreeV :: (Eq a1, Num a, Num a1) => Graph a1 -> a
countOddDegreeV (Graph g) = sum [ 1 | v <- vertices2list $ getVerticesG (Graph g), odd $ degreeG (Graph g) (Vertices [v]) ]


-- Finding number of vertices with even degree
{-
	Usage:
		g1
		>>> Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

		countEvenDegreeV g1
		>>> 2
-}
countEvenDegreeV :: (Eq a1, Num a, Num a1) => Graph a1 -> a
countEvenDegreeV (Graph g) = sum [ 1 | v <- vertices2list $ getVerticesG (Graph g), even $ degreeG (Graph g) (Vertices [v]) ]


-- Finding if a Graph conatains a Euler Path but not a Euler circuit
{-
	Usage:
		g1
		>>> Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

		hasEulerPathNotCircuitG g1
		>>> True
-}
hasEulerPathNotCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasEulerPathNotCircuitG (Graph g) = countOddDegreeV (Graph g) == 2


-- Finding if a GraphMatrix contains a Euler Path but not a Euler circuit
{-
	Usage:
		gm
		>>>	0	4	1	5
			0	0	0	0
			0	6	0	0
			0	0	0	0

		hasEulerPathNotCircuitGM gm
		>>> True
-}
hasEulerPathNotCircuitGM :: Integral a => GraphMatrix a -> Bool
hasEulerPathNotCircuitGM (GraphMatrix gm) = hasEulerPathNotCircuitG (convertGM2G (GraphMatrix gm))


-- Finding if a Graph contains a Hamiltonian Circuit
{-
	Usage:
		g2
		>>> Graph ({1,2,3,4,5},{(1,2,1),(1,3,1),(1,5,1),(2,1,1),(2,3,1),(2,5,1),(3,1,1),(3,2,1),(3,4,1),(3,5,1),(4,3,1),(4,5,1),(5,1,1),(5,2,1),(5,3,1),(5,4,1)})

		hasHamiltonianCircuitG g2
		>>> True
-}
hasHamiltonianCircuitG :: (Eq a, Num a) => Graph a -> Bool
hasHamiltonianCircuitG (Graph g) = and [degreeG (Graph g) (Vertices [v]) >= (numVerticesG (Graph g) `div` 2) | v <- vertices2list $ getVerticesG (Graph g), numVerticesG (Graph g) >= 3]


-- Finding if a GraphMatrix contains a Hamiltonian Circuit
{-
	Usage:
		gm
		>>>	0	1	1	0	1
			1	0	1	0	1
			1	1	0	1	1
			0	0	1	0	1
			1	1	1	1	0

		hasHamiltonianCircuitGM gm
		>>> True
-}
hasHamiltonianCircuitGM :: Integral a => GraphMatrix a -> Bool
hasHamiltonianCircuitGM (GraphMatrix gm) = hasHamiltonianCircuitG (convertGM2G (GraphMatrix gm))


-- Checking if a Graph is a subgraph
{-
	Usage:
		g1
		>>> Graph ({1,2,3,4},{(1,2,4),(1,3,1),(1,4,5),(3,2,6)})

		g2
		>>> Graph ({1,3},{(1,3,1)})

		isSubgraphG g2 g1
		>>> True

		isSubgraphG g1 g2
		>>> False
-}
isSubgraphG :: (Num a, Ord a) => Graph a -> Graph a -> Bool
isSubgraphG (Graph g1) (Graph g2) = (e1 `isSubset` e2) && (v1 `isSubset` v2)
  where
    isSubset set1 set2 = null [e | e <- (L.sort . L.nub) set1, e `notElem` (L.sort . L.nub) set2]
    e1 = edges2list $ getEdgesG (Graph g1)
    e2 = edges2list $ getEdgesG (Graph g2)
    v1 = vertices2list $ getVerticesG (Graph g1)
    v2 = vertices2list $ getVerticesG (Graph g2)


-- Checking if a GraphMatrix is a subgraph
{-	
	Usage:
		grm1
		>>>	0	4	1	5
			0	0	0	0
			0	6	0	0
			0	0	0	0

		grm2
		>>>	0	4
			0	0

		isSubgraphGM grm2 grm1
		>>> True

		isSubgraphGM grm1 grm2
		>>> False
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
