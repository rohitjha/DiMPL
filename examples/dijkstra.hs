import Data.List
import qualified Data.List.Key as K	--cabal install utility-ht
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)
import MPL.GraphTheory.Graph


buildGraph :: (Ord a) => [(a, a, Int)] -> Map a [(a, Int)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]


getEdgeList (Graph g) = edges2list $ getEdgesG (Graph g)


dijkstra :: (Ord a) => a -> Map a [(a, Int)] -> Map a (Int, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else (2^31), Nothing)) | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e


shortestPath :: Ord a => a -> a -> Graph a -> [a]--Map a [(a, Float)] -> [a]
shortestPath from to (Graph graph) = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from (buildGraph (getEdgeList (Graph graph))) ! x)


v = Vertices [1..5]
e = Edges [(1,3,2),(1,4,6),(2,1,3),(2,4,8),(3,4,7),(3,5,5),(4,5,10)]
g = Graph (v,e)


main = putStrLn $ show $ shortestPath 1 5 g

{-
main :: IO ()
main = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3),
                              ('b','d',8), ('c','d',7), ('c','e',5),
                              ('d','e',10)]
          print $ shortestPath 'a' 'e' g -- == "ace"
-}

{-
	Usage:
		let v = Vertices [1..5]
		let e = Edges [(1,3,2),(1,4,6),(2,1,3),(2,4,8),(3,4,7),(3,5,5),(4,5,10)]
		let g = Graph (v,e)
		shortestPath 1 5 g
		>>> [1,3,5]
-}
