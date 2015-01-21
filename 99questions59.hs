{-
7 Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

Example:

?- hbal_tree(3,T).
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell:

*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}

--import Tree

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

isHeightBalancedTree :: Tree a -> Bool
isHeightBalancedTree Empty = True
isHeightBalancedTree (Branch _ l r) = abs(height l - height r) <= 1
                                      && isHeightBalancedTree l && isHeightBalancedTree r
height :: Tree a -> Int
height leaf = 1
height (Branch _ l r) = 1 + max (height l ) (height r)  

makeTrees :: a -> Int ->[Tree a]
makeTrees _ 0 = []
makeTrees c 1 = [leaf c]
makeTrees c n = lonly ++ ronly ++ landr
  where lonly = [Branch c t Empty | t <- smallTree]
        ronly = [Branch c Empty t | t <- smallTree]
        landr = concat [[Branch c l r | l <- fst lrtrees, r <- snd lrtrees] | lrtrees <- treeMinusTwo]
        smallTree = makeTrees c (n-1)
        treeMinusTwo = [(makeTrees c num, makeTrees c (n-1-num)) | num <- [0..n-2]]

main = putStrLn $ concatMap (\t -> show t ++ "\n") hbalTrees
  where hbalTrees = filter isHeightBalancedTree(makeTrees 'x' 4)


