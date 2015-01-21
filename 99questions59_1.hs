data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

hbalTree x = map fst . hbalTree'
  where hbalTree' 0 = [(Empty,0)]
        hbalTree' 1 = [(leaf x, 1)]
        -- '++' is concatation, hbalTree has list type, this is a critical step
        -- for a h-balance tree with height 'H', the left or right subtree should has a height
        -- (H-1,H-2),(H-1,H-1),(H-2,H-1), so after concatation of (H-2) and (H-1), then 't' would 
        -- have all posibilities of the subtree.
        hbalTree' n = let t = hbalTree' (n-2) ++ hbalTree' (n-1) 
                      in [(Branch x lb rb, h) | (lb,lh) <-t, (rb,rh) <-t, 
                          let h = 1 + max lh rh, h==n]

hbalTree1 :: a -> Int -> [Tree a]
hbalTree1 x 0 = [Empty]
hbalTree1 x 1 = [leaf x]
hbalTree1 x h = [Branch x l r | 
                  (hl,hr)<-[(h-2,h-1),(h-1,h-1),(h-1,h-2)], 
                  l <- hbalTree1 x hl, r <- hbalTree1 x hr]

