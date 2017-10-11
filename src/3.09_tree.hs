
--
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show,Eq)

--
treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ lhs rhs) = 1 + max (treeDepth lhs) (treeDepth rhs)

--
treeSum :: (Num a) => Tree a -> a
treeSum Leaf = 0
treeSum (Node val lhs rhs) = val + (treeSum lhs) + (treeSum rhs)

--
isSortedTree :: (Ord a) => Tree a -> a -> a -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node val lhs rhs) mn mx = 
        let
                lhsSorted = isSortedTree lhs mn val
                rhsSorted = isSortedTree rhs val mx
        in
                val >= mn && val < mx && lhsSorted && rhsSorted

--
isSorted :: (Ord a) => Tree a -> Bool
isSorted = testTree (\x->True) 
        
testTree :: (Ord a) => (a->Bool) -> Tree a -> Bool
testTree _ Leaf = True
testTree pred (Node x lhs rhs) = pred(x) && testTree (<x) lhs && testTree (>=x) rhs


--
addNewMax :: (Integral a) => Tree a -> Tree a
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x lhs Leaf) = Node x lhs (Node (x+1) Leaf Leaf)
addNewMax (Node x lhs rhs) = Node x lhs (addNewMax rhs)


insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node val lhs rhs)
        | x < val       = Node val (insert x lhs) rhs
        | otherwise     = Node val lhs (insert x rhs)

toList :: Tree a -> [a]
toList Leaf = []
toList (Node val lhs rhs) = (toList lhs) ++ val:(toList rhs)

toLst2 :: Tree a -> [a]
toLst2 Leaf = []
toLst2 (Node val lhs rhs) = (toLst2 rhs) ++ val:(toLst2 lhs)

fromList :: Ord a => [a] -> Tree a
fromList xs = fromListHelp xs Leaf

fromListHelp :: Ord a => [a] -> Tree a -> Tree a
fromListHelp [] a = a
fromListHelp (x:xs) a = fromListHelp xs (insert x a)

toTree :: Ord a => [a] -> Tree a
--toTree = foldr insert Leaf
toTree = foldl (flip insert) Leaf

--
-- *Main> list (\x y z -> x:y ++ z) (toTree [3,9,7,10,2,2,1])
-- [3,2,1,2,9,7,10]
-- *Main> list (\x y z -> y ++ x:z) (toTree [3,9,7,10,2,2,1])
-- [1,2,2,3,7,9,10]
-- *Main> list (\x y z -> y ++ z ++ [x]) (toTree [3,9,7,10,2,2,1])
-- [1,2,2,7,10,9,3]
--
list :: (a->[a]->[a]->[a]) -> Tree a -> [a]
list _ Leaf = []
list order (Node val lhs rhs) = order val (list order lhs) (list order rhs)


treezip:: (Tree a) -> (Tree b) -> (Tree(a,b))
treezip (Node valA lhsA rhsA) (Node valB lhsB rhsB) =
        Node (valA, valB) (treezip lhsA lhsB) (treezip rhsA rhsB)
treezip _ _ = Leaf


treeunzip:: (Tree(a,b)) -> (Tree a, Tree b)
treeunzip Leaf = (Leaf , Leaf)
treeunzip (Node (x,y) lhs rhs) = 
        let 
                unzipLhs = treeunzip lhs
                unzipRhs = treeunzip rhs
        in
                ( Node x (fst unzipLhs) (fst unzipRhs) , Node y (snd unzipLhs) (snd unzipRhs) )


-- (treezip (fst (treeunzip x)) (snd (treeunzip x))) == x given x::Tree(a,b)
-- *Main> t = toTree [3,9,8,7,0,4,2,3,1]
-- *Main> t' = toTree ["3three","9nine","8eight","7seven","0zero","4four","2two","3three","1one"]
-- *Main> zt = treezip t t'
-- *Main> (treezip (fst (treeunzip zt)) (snd (treeunzip zt))) == zt
-- True
-- *Main> 

