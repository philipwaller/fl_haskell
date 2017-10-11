-- Define 'Tree' data type
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show,Eq)

-- Find the maximum depth of a Tree
treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ lhs rhs) =
        1 + max (treeDepth lhs) (treeDepth rhs)

-- Sum the values of a Tree of Numbers
treeSum :: Num a => Tree a -> a
treeSum Leaf = 0
treeSum (Node val lhs rhs) =
        val + treeSum lhs + treeSum rhs

-- Test for sorted tree
isSortedTree :: Ord a => Tree a -> Bool
isSortedTree = isSortedTree' (\x -> True)

-- Helper test for a sorted tree
isSortedTree' :: Ord a => (a -> Bool) -> Tree a -> Bool
isSortedTree' _ Leaf = True
isSortedTree' pred (Node val lhs rhs) =
        pred val && isSortedTree' (<val) lhs && isSortedTree' (>=val) rhs

-- Add a new maximun element to a Tree of Integers
addNewMax :: Integral a => Tree a -> Tree a
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node val lhs Leaf) = Node val lhs (Node (val+1) Leaf Leaf)
addNewMax (Node val lhs rhs) = Node val lhs (addNewMax rhs)

-- Insert a value into an ordered List
insertVal :: Ord a => a -> Tree a -> Tree a
insertVal val Leaf = Node val Leaf Leaf
insertVal val (Node x lhs rhs)
        | val < x = Node x (insertVal val lhs) rhs
        | otherwise = Node x lhs (insertVal val rhs)

-- Default convertion of Tree to List uses Pre-Order
toList :: Ord a => Tree a -> [a]
toList = preOrder

-- Convert Tree to Prer-Order List
preOrder :: Ord a => Tree a -> [a]
preOrder = toList' (\x y z -> x:y ++ z)

-- Convert Tree to In-Order List
inOrder :: Ord a => Tree a -> [a]
inOrder = toList' (\x y z -> y ++ x:z)

-- Convert Tree to Post-Order List
postOrder :: Ord a => Tree a -> [a]
postOrder = toList' (\x y z -> y ++ z ++ [x])

-- Generic convertion of Tree to List
toList' :: (a -> [a] -> [a] -> [a]) -> Tree a -> [a]
toList' _ Leaf = []
toList' order (Node val lhs rhs) = 
        order val (toList' order lhs) (toList' order rhs)

-- Create Tree from List
fromList :: Ord a => [a] -> Tree a
fromList = foldr insertVal Leaf

-- Zip Tree
treezip:: (Tree a) -> (Tree b) -> (Tree(a,b))
treezip (Node valA lhsA rhsA) (Node valB lhsB rhsB) =
        Node (valA, valB) (treezip lhsA lhsB) (treezip rhsA rhsB)
treezip _ _ = Leaf

-- Unzip Tree
treeunzip:: (Tree(a,b)) -> (Tree a, Tree b)
treeunzip Leaf = (Leaf , Leaf)
treeunzip (Node (x,y) lhs rhs) = 
           ( Node x lhsA rhsA , Node y lhsB rhsB )
           where
                (lhsA,lhsB) = treeunzip lhs
                (rhsA,rhsB) = treeunzip rhs

