packList :: [Int] -> [(Int,Int)]
packList = foldr helper [] 
  where helper x [] = [(x ,1)] 
        helper x (y@(a,b):ys)
               | x == a = (a,1+b) : ys  
               | otherwise = (x,1) : y : ys


