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
capital :: String -> String
capital "" = " Empty string "
capital all@(x:xs) = " The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: ( RealFloat a ) => a-> a-> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 25.5 = "You're normal"
    | weight / height ^ 2 <= 30.5 = "You're fat "
    | otherwise = " you are a whale, cong "

calcBmis :: ( RealFloat a ) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <-xs]
    where bmi weight height = weight / height ^ 2
