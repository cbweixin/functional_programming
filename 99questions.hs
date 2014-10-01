-- problem 1 find the last element of a list
tail' :: [a] -> a
tail' [] =  error " can not call tail on an empty list "
tail' [x] = x
tail' (_:xs) = tail' xs

-- use fold
--tail_2 = foldr1 (const id)
-- I don't understand this solution that much
tail_2 = foldr1 (const id)

-- another usage of foldr1
tail_3 = foldr1 (flip const)

----------------------------------------------------------
-- problme 2
-- find the last but one element of a list
-- example
-- myButLast [1,2,3,4]
-- 3
myButLast :: [a] -> a
myButLast [] = error " can not call myButLast on empty list "
myButLast [x] = error " can not call myButLast on single element list "
myButLast (x:y:[]) = x
myButLast ( _ : xs ) = myButLast xs

-- another way
myButLast_1 [x,_] = x
myButLast_1 (_:xs) = myButLast_1 xs

-- another way
-- this is the best
-- tail [1,2,3,4] = [2,3,4]
-- reverse [1,2,3,4] = [4,3,2,1]
myButLast_2 = head . tail .reverse

---------------------------------------------------------
--problem 3
--find the kth element of a list , the first elment of the list is 1
elementAt_1 :: [a]->Int->a
-- the first element should be x if the lsit in such a form (x:_)
elementAt_1 (x:_) 1 = x  
-- any index for a empty list would be outofbound error
elementAt_1 [] _ = error "Index out of bounds"
elementAt_1 (_:xs) k 
	| k < 1 = error "index out of bounds"
	| otherwise = elementAt_1 xs (k-1)

-- a better solution
-- just using operator !!
-- [1,3,2] !!0 = 1, notice the starting index is 0 instead of 1, so we need to modify it to make it work
elementAt :: [a]->Int->a
elementAt list i 
	| i > 0  = list !! (i-1)
	| otherwise  = error "starting index should from 1"


---------------------------------------------------------
--problem 4 find the number of elements of a list
--myLength[1,2,3] = 3
--myLength "aaa,a" = 5
myLength_1 :: [a] -> Int
myLength_1 (x:[]) = 1
myLength_1 (_:xs) = 1 + myLength_1 xs 

-- using accumulator
myLength_2 :: [a] -> Int
myLength_2 list = myLength_acc list 0
	where
			myLength_acc [] n = n
			myLength_acc (_:xs) n = myLength_acc xs (n + 1)

-- using fold
myLength_3 :: [a] -> Int
myLength_3 = foldl (\acc _ -> acc + 1) 0 


---------------------------------------------------------
-- problem 5, reverse a list
reverse_1 :: [a] -> [a]
reverse_1 = foldl(\acc x -> (x:acc)) [] 

-- another way of foldl, more concise
reverse_2 :: [a] -> [a]
reverse_2 = foldl(flip (:)) []
