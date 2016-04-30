{-|
Module      : Tree
Description : Tree module for the MPL DSL
Copyright   : (c) Rohit Jha, 2015
License     : BSD2
Maintainer  : rohit305jha@gmail.com
Stability   : Stable

Functionality for
    * Binary Tree data structure
    * Inorder, Preorder and Postorder tree traversals
    * Inserting nodes in an element
    * Finding if an element is in a tree
    * Calculating Reflection of a tree (mirroring)
    * Finding Height of a tree
    * Finding depth of a node
    * Determining Size of a tree (number of nodes)
    * Checking if a tree is balanced
-}

module Tree
(
    BinTree(..),
    inorder,
    preorder,
    postorder,
    singleton,
    addNode,
    hasValue,
    reflect,
    height,
    depth,
    size,
    isBalanced
)
where


import System.Environment

{-|
    The 'BinTree' datatype is used for representing binary trees.
-}
data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving (Eq, Show)


-- SAMPLE TREES --
tree1 = Leaf

tree2 =
    Node 4
       (Node 2
          (Node 1 Leaf Leaf)
          (Node 3 Leaf Leaf))
       (Node 7
          (Node 5
             Leaf
             (Node 6 Leaf Leaf))
          (Node 8 Leaf Leaf))


{-|
    The 'inorder' function returns the nodes of a 'BinTree' after inorder traversal.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> inorder tree2
    [1,2,3,4,5,6,7,8]
-}
inorder :: BinTree a -> [a]
inorder Leaf = []
inorder (Node x t1 t2) = inorder t1 ++ [x] ++ inorder t2


{-|
    The 'preorder' function returns the nodes of a 'BinTree' after preorder traversal.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> preorder tree2
    [4,2,1,3,7,5,6,8]
-}
preorder :: BinTree a -> [a]
preorder Leaf = []
preorder (Node x t1 t2) = [x] ++ preorder t1 ++ preorder t2


{-|
    The 'postorder' function returns the nodes of a 'BinTree' after postorder traversal.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> postorder tree2
    [1,3,2,6,5,8,7,4]
-}
postorder :: BinTree a -> [a]
postorder Leaf = []
postorder (Node x t1 t2) = postorder t1 ++ postorder t2 ++ [x]


{-|
    The 'singleton' function returns a single childless node.

    For example:

    >>> singleton 10
    Node 10 Leaf Leaf

    >>> singleton 'a'
    Node 'a' Leaf Leaf

    >>> singleton 'A'
    Node 'A' Leaf Leaf
-}
singleton :: a -> BinTree a
singleton x = Node x Leaf Leaf


{-|
    The 'addNode' function inserts a node into a 'BinTree'.

    The function takes two arguments - the first is the value to be inserted and the second is the 'BinTree' into which the value is to be inserted.

    For example:

    >>> addNode 10 (Leaf)
    Node 10 Leaf Leaf

    >>> addNode 10 tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf (Node 10 Leaf Leaf)))
-}
addNode :: ( Ord a ) => a -> BinTree a -> BinTree a
addNode x Leaf = singleton x
addNode x ( Node a left right )
    | x == a = Node x left right            --the element is equal to root node
    | x < a  = Node a ( addNode x left ) right   --element smaller than root node, inserted on it's left
    | x > a  = Node a left ( addNode x right )   --element smaller than root node, inserted on it's right


{-|
    The 'hasValue' function checks if an elements exists in a 'BinTree' by using binary search.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> hasValue 5 tree2
    True

    >>> hasValue 10 tree2
    False
-}
hasValue :: ( Ord a ) => a -> BinTree a -> Bool
hasValue x Leaf = False
hasValue x ( Node a left right )
    | x == a = True
    | x < a = hasValue x left
    | x > a = hasValue x right


{-|
    The 'depth' function returns the depth of a node in a 'BinTree'.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> depth 1 tree2 0
    2

    >>> depth 2 tree2 0
    1

    >>> depth 4 tree2 0
    0

    >>> depth 8 tree2 0
    2
-}
depth :: (Ord a) => a -> BinTree a -> Integer -> Integer
depth x Leaf n = n
depth x (Node a left right) n
    | x == a = n
    | x < a = depth x left (n+1)
    | x > a = depth x right (n+1)


{-|
    The 'reflect' function returns the mirror reflect of a 'BinTree'.

    For example:

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> inorder tree2
    [1,2,3,4,5,6,7,8]

    >>> reflect tree2
    Node 4 (Node 7 (Node 8 Leaf Leaf) (Node 5 (Node 6 Leaf Leaf) Leaf)) (Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))

    >>> inorder (reflect tree2)
    [8,7,6,5,4,3,2,1]
-}
reflect :: BinTree a -> BinTree a
reflect Leaf = Leaf
reflect (Node n l r) = Node n (reflect r) (reflect l)


{-|
    The 'height' function returns the height of a 'BinTree'.

    For example:

    >>> tree1
    Leaf

    >>> height tree1
    0

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> height tree2
    4
-}
height :: BinTree a -> Integer
height Leaf = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2)


{-|
    The 'size' function returns the number of nodes in a 'BinTree'. Leaf nodes are not counted.

    For example:

    >>> tree1
    Leaf

    >>> size tree1
    0

    >>> tree2
    Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

    >>> size tree2
    8
-}
size :: BinTree a -> Int
size Leaf = 0
size (Node x t1 t2) = 1 + size t1 + size t2


{-|
    The 'isBalanced' function checks is a 'BinTree' is properly balanced.

    For example:

    >>> balanced tree2
    False

    >>> balanced tree1
    True
-}
isBalanced :: BinTree a -> Bool
isBalanced Leaf = True
isBalanced (Node x t1 t2) = isBalanced t1 && isBalanced t2 && (height t1 == height t2)


edges :: BinTree a -> [(a,a)]
edges Leaf = []
edges (Node a Leaf Leaf) = []
edges (Node a (Node b t1 t2) (Leaf)) = [(a,b)] ++ edges (Node b t1 t2)
edges (Node a (Leaf) (Node b t1 t2)) = [(a,b)] ++ edges (Node b t1 t2)
edges (Node a (Node b t1 t2) (Node c t3 t4)) = [(a,b), (a,c)] ++ edges (Node b t1 t2) ++ edges (Node c t3 t4)


directedEdges :: Show a => BinTree a -> String
directedEdges a
  | (length (edges a) > 0) = foldl1 (++) [ (show x) ++ " -> " ++ (show y) ++ "\n" | (x,y) <- edges a ]
  | otherwise = []


addDigraphDotStructure :: String -> String
addDigraphDotStructure s = "digraph G\n{\n" ++ s ++ "}"


writeTreeToFile :: (Show a) => String -> BinTree a -> IO()
writeTreeToFile file a = writeFile file (addDigraphDotStructure $ directedEdges a)
