-- Kruskal's algorithm for Minimum Spanning Tree

import Data.List(sort)
import Data.Set (Set, member, fromList, insert, union)
import Graph


data Edge a = Edge a a Double deriving Show

instance (Eq a) => Eq (Edge a) where
  Edge x1 y1 z1 == Edge x2 y2 z2 = x1 == x2 && y1 == y2 && z1 == z2

instance Eq a => Ord (Edge a) where
  (Edge _ _ x) `compare` (Edge _ _ y) = x `compare` y


kruskal :: Ord a => [Edge a] -> [Edge a]
kruskal = fst . foldl mst ([],[]) . sort


mst :: Ord a => ([Edge a],[Set a]) -> Edge a -> ([Edge a],[Set a])
mst (es, sets) e@(Edge p q _) = step $ extract sets where
   step (rest, Nothing, Nothing) = (e : es, fromList [p,q] : rest)
   step (rest, Just ps, Nothing) = (e : es, q `insert` ps : rest)
   step (rest, Nothing, Just qs) = (e : es, p `insert` qs : rest)
   step (rest, Just ps, Just qs) | ps == qs = (es, sets) --circle
                                 | otherwise = (e : es, ps `union` qs : rest)
   extract = foldr f ([], Nothing, Nothing) where
       f s (list, setp, setq) =
            let list' = if member p s || member q s then list else s:list
                setp' = if member p s then Just s else setp
                setq' = if member q s then Just s else setq
            in (list', setp', setq') 

main = putStrLn $ show $ kruskal [(Edge 1 2 3), (Edge 1 3 5), (Edge 2 3 3), (Edge 3 4 1)]
{-
	Usage:
		let e = [(Edge 1 2 3), (Edge 1 3 5), (Edge 2 3 3), (Edge 3 4 1)]
		kruskal e
		>>> [Edge 2 3 3.0,Edge 1 2 3.0,Edge 3 4 1.0]
-}
