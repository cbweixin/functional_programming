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


---------------------------------------------------------
-- problme 6 palindrome
isPalindrome_1 :: (Eq a) => [a] -> Bool
isPalindrome_1 xs = xs == reverse xs

isPalindrome_2 :: (Eq a) => [a] -> Bool
isPalindrome_2 [] = True
isPalindrome_2 [x] = True
isPalindrome_2 xs = head xs == last xs && ( isPalindrome_2 $ tail $ init xs )


---------------------------------------------------------
-- problme 7 flatten a nested list
data NestedList a = Elem a | List [NestedList a]
flattern1 :: NestedList a -> [a]
flattern1 (Elem a) = [a]
flattern1 (List (x:xs)) = flattern1 x ++ flattern1 (List xs)
flattern1 (List []) = []

flattern2 :: NestedList a -> [a]
flattern2 a = flt a []
  where flt (Elem x) xs = x:xs
        flt (List (x:ls)) xs = flt x (flt (List ls) xs)
        flt (List []) xs = xs

{-
flatten2 :: NestedList a -> [a]
flatten2 a = flt a []
  where flt (Elem x)      xs = x:xs
        flt (List (x:ls)) xs = flt x (flt (List ls) xs)
        flt (List [])     xs = xs
-}
---------------------------------------------------------
-- problme 8 eliminate consective duplicate list elements 
compress1 :: Eq a => [a] -> [a]
compress1 [] = []
compress1 [x] = [x]
compress1 (x:xs) 
  | x == head xs = compress1 xs 
  | otherwise = x : compress1 xs

{-
compress2 :: Eq a => [a] -> [a]
compress2 xs = map head $ group xs
-}

-- using foldr
compress3 :: Eq a => [a] -> [a]
compress3 = foldr skipDups []
  where  skipDups x [] = [x]
         skipDups x acc
          | x == head acc = acc
          | otherwise = x:acc
          
---------------------------------------------------------
-- problme 9 pack conseutive duplicate elements 
--
pack1 :: Eq a => [a] -> [[a]]
pack1 = foldr packDups []
  where packDups x [] = [[x]]
        packDups x (y:acc)
          | x == (head y) = ((x:y):acc) 
          | otherwise = [x]: y : acc

---------------------------------------------------------
-- problme 10 use the result of problem of P09 to implement a so-called
-- run-length encoding data compression method
-- example encode "aaabccc" -> [(3,'a'), (1,'b'),(3,'c')]
--
encode1 :: Eq a => [a] -> [(Int,a)]
--encode1 xs = map (\x -> (length x, head x)) $ pack1 xs
encode1 = map (\x -> (length x, head x)) . pack1 

---------------------------------------------------------
-- problme 11 use the result of problem of P09 to implement a so-called
-- modify run-length encoding
-- encodemodified "aaaabccaadee"
-- [Multiple 4 'a', single 'b',.., Multiple 2 'e'] 

data ListItem a = Single a | Multiple Int a deriving (Show)
encodemodified_1 :: Eq a => [a] -> [ListItem a]
encodemodified_1  = map encodeHelper . encode1
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x


---------------------------------------------------------
-- problme 12 
-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:

-- P12> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodemodified_1 :: [ListItem a] ->[a] 
decodemodified_1 = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple 2 x) = [x] ++  decodeHelper (Single x)
    -- notice the (n-1) , you can never missing the bracket '()', otherwise you could 
    -- get error
    decodeHelper (Multiple n x) = [x] ++  decodeHelper (Multiple (n-1) x)

-- a more elegant way by using built-in 'replicate' function
decodemodified_2 :: [ListItem a] ->[a] 
decodemodified_2 = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

-- a navie way by using foldl
decodemodified_3 :: [ListItem a] ->[a] 
decodemodified_3 = foldl(\x y->x ++ decodeHelper y)[]
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

{- problem 13
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
encode_2 :: Eq a => [a] -> [(Int,a)]
encode_2 = foldr helper [] 
  where 
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
           | x == b = (1+a,x):ys
           | otherwise = (1,x):y:ys

encodeDirect_1 :: Eq a => [a]->[ListItem a]
encodeDirect_1 = map encodeHelper . encode_2
  where 
    encodeHelper (1,x) = Single x
    encodeHelper (n,x) = Multiple n x

{-
4 Problem 14
(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:

> dupli [1, 2, 3]
[1,1,2,2,3,3]
Solutions
-}
dupli_1 :: [a] -> [a]
dupli_1 = concatMap (replicate 2)

dupli_2 :: [a] -> [a]
dupli_2 = foldr helper []
  --where helper x xs = replicate 2 x ++ xs
  where helper x xs = x : x : xs
