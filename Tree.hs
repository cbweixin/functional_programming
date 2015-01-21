module Tree
(symmetric,
 cbalTree,
 symCbalTree,
 makeTrees
 ) where


data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n-1) `quotRem` 2 
  in [Branch 'x' left right | i  <- [q..q+r],
                              left <- cbalTree i,
                              right <- cbalTree (n-i-1)]

symmetric :: Tree a -> Bool
symmetric Empty  = True
-- the code below is not necessary since mirror empty empty = true
-- after a recursive call, leaf would fall into this pattern.
--symmetric leaf = True
symmetric (Branch _ l r) = mirror l r
  where mirror Empty Empty = True
        mirror (Branch _ a b) (Branch _ x y ) = mirror a y && mirror b x 
        mirror _ _ = False

symCbalTree = filter symmetric . cbalTree  

makeTrees :: a -> Int ->[Tree a]
makeTrees _ 0 = []
makeTrees c 1 = [leaf c]
makeTrees c n = lonly ++ ronly ++ landr
  where lonly = [Branch c t Empty | t <- smallTree]
        ronly = [Branch c Empty t | t <- smallTree]
        landr = concat [[Branch c l r | l <- fst lrtrees, r <- snd lrtrees] | lrtrees <- treeMinusTwo]
        smallTree = makeTrees c (n-1)
        treeMinusTwo = [(makeTrees c num, makeTrees c (n-1-num)) | num <- [0..n-2]]

