head' :: [a]->a
head' [] = error " Can't call head on an empty list, dummy !"
head' (x:_) = x
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ "and" ++ show y
tell (x:y:_) = "The list has more than two element: " ++ show x ++ "and" ++ show y
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

