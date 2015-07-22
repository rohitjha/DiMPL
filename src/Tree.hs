{-
------------------------
| MPL Module for Trees |
------------------------

Functionality for
    -> Binary Tree data structure
    -> Inorder, Preorder and Postorder tree traversals
    -> Inserting nodes in an element
    -> Finding if an element is in a tree
    -> Calculating Reflection of a tree (mirroring)
    -> Finding Height of a tree
    -> Finding depth of a node
    -> Determining Size of a tree (number of nodes)
    -> Checking if a tree is balanced

Author: Ashmee Pawar, Rohit Jha, Alfy Samuel
Version: 0.1
Date: 31 Jan 2013
-}

module Tree
(
    BinTree(..),
    inorder,
    preorder,
    postorder,
    singleton,
    treeInsert,
    treeElem,
    reflect,
    height,
    depth,
    size,
    isBalanced
)
where


-- Binary tree representation
data BinTree a = Leaf | Node a (BinTree a) (BinTree a) deriving (Eq, Show)

{-
instance Show a => Show (BinTree a) where
    show (Leaf) = "Leaf"
    show (Node a Leaf Leaf) = show a--L.intercalate "\n" $ map (L.intercalate "\t" . map show) a-}


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


--Inorder tree traversal
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        inorder tree2
        >>> [1,2,3,4,5,6,7,8]
-}
inorder :: BinTree a -> [a]
inorder Leaf = []
inorder (Node x t1 t2) = inorder t1 ++ [x] ++ inorder t2


-- Preorder tree traversal
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        preorder tree2
        >>> [4,2,1,3,7,5,6,8]
-}
preorder :: BinTree a -> [a]
preorder Leaf = []
preorder (Node x t1 t2) = [x] ++ preorder t1 ++ preorder t2


-- Postorder tree traversal
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        postorder tree2
        >>> [1,3,2,6,5,8,7,4]
-}
postorder :: BinTree a -> [a]
postorder Leaf = []
postorder (Node x t1 t2) = postorder t1 ++ postorder t2 ++ [x]


-- Single childless node
{-
    Usage:
        singleton 10
        >>> Node 10 Leaf Leaf

        singleton 'a'
        >>> Node 'a' Leaf Leaf

        singleton 'A'
        >>> Node 'A' Leaf Leaf
-}
singleton :: a -> BinTree a
singleton x = Node x Leaf Leaf


-- Insert an element into a Tree
{-
    Usage:
        treeInsert 10 (Leaf)
        >>> Node 10 Leaf Leaf

        treeInsert 10 tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf (Node 10 Leaf Leaf)))
-}
treeInsert :: ( Ord a ) => a -> BinTree a -> BinTree a
treeInsert x Leaf = singleton x
treeInsert x ( Node a left right )
    | x == a = Node x left right            --the element is equal to root node
    | x < a  = Node a ( treeInsert x left ) right   --element smaller than root node, inserted on it's left
    | x > a  = Node a left ( treeInsert x right )   --element smaller than root node, inserted on it's right


-- To check if an element lies within a tree (Binary Tree search)
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        treeElem 5 tree2
        >>> True

        treeElem 10 tree2
        >>> False
-}
treeElem :: ( Ord a ) => a -> BinTree a -> Bool
treeElem x Leaf = False 
treeElem x ( Node a left right )
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


-- Finding depth of a node
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        depth 1 tree2 0
        >>> 2

        depth 2 tree2 0
        >>> 1

        depth 4 tree2 0
        >>> 0

        depth 8 tree2 0
        >>> 2
-}
depth :: (Ord a) => a -> BinTree a -> Integer -> Integer
depth x Leaf n = n
depth x (Node a left right) n
    | x == a = n
    | x < a = depth x left (n+1)
    | x > a = depth x right (n+1)


-- Mirror reflection of a tree
{-
    Usage:
        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        reflect tree2
        >>> Node 4 (Node 7 (Node 8 Leaf Leaf) (Node 5 (Node 6 Leaf Leaf) Leaf)) (Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))
-}
reflect :: BinTree a -> BinTree a
reflect Leaf = Leaf
reflect (Node n l r) = Node n (reflect r) (reflect l)


-- Height of a tree
{-
    Usage:
        tree1
        >>> Leaf

        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        height tree1
        >>> 0

        height tree2
        >>> 4
-}
height :: BinTree a -> Integer
height Leaf = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2)


-- Size of a tree (number of nodes)
{-
    Usage:
        tree1
        >>> Leaf

        size tree1
        >>> 0

        tree2
        >>> Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 7 (Node 5 Leaf (Node 6 Leaf Leaf)) (Node 8 Leaf Leaf))

        size tree2
        >>> 8
-}
size :: BinTree a -> Int
size Leaf = 0
size (Node x t1 t2) = 1 + size t1 + size t2


-- To check if a tree is balanced
{-
    Usage:
        balanced tree2
        >>> False

        balanced tree1
        >>> True
-}
isBalanced :: BinTree a -> Bool
isBalanced Leaf = True
isBalanced (Node x t1 t2) = isBalanced t1 && isBalanced t2 && (height t1 == height t2)
