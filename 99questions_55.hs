data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

main = putStrLn $ concatMap (\t -> show t ++ "\n") balTrees
  where balTrees = filter isBalancedTree(makeTrees 'x' 4)

isBalancedTree :: Tree a -> Bool
isBalancedTree Empty = True
isBalancedTree (Branch _ l r) = abs(countBranches l - countBranches r) <= 1
                                && isBalancedTree l && isBalancedTree r

isBalancedTree _ = False

countBranches :: Tree a -> Int
countBranches Empty = 0
countBranches (Branch _ l r) = 1 + countBranches l + countBranches l

makeTrees :: a -> Int ->[Tree a]
makeTrees _ 0 = []
makeTrees c 1 = [leaf c]
makeTrees c n = lonly ++ ronly ++ landr
  where lonly = [Branch c t Empty | t <- smallTree]
        ronly = [Branch c Empty t | t <- smallTree]
        landr = concat [[Branch c l r | l <- fst lrtrees, r <- snd lrtrees] | lrtrees <- treeMinusTwo]
        smallTree = makeTrees c (n-1)
        treeMinusTwo = [(makeTrees c num, makeTrees c (n-1-num)) | num <- [0..n-2]]

