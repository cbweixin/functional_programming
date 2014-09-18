-- use 'foldl' to do summation
-- \acc x -> is a binary lamda function
sum_1 :: (Num a) => [a] -> a
sum_1 xs = foldl(\acc x -> acc + x ) 0 xs

-- a more succint way
-- '(+)' is the same as (\acc x -> acc + x)
-- also 'foldl (+) 0' would 
sum_2 :: (Num a) => [a] -> a
sum_2 = foldl (+) 0

-- check an element to see if it belongs to a list or not
elem_1 :: (Eq a) => a -> [a] -> Bool
elem_1 y ys = foldl (\acc x -> if x == y then True else acc) False ys 

-- an example of use 'foldr'
map_1 :: (a->b) -> [a] -> [b]
map_1 f xs = foldr (\x acc -> f x : acc)[] xs

-- same function use 'foldl'
--  the above map which uses 'foldr' is faster than 'foldl' because 
--  f x : acc is faster than acc ++ [f x]
--  : is prepend, ie ,append at the head 
--  ++ is the concatenation, append 2 lists
map_2 :: (a->b) -> [a] -> [b] 
map_2 f xs = foldl (\acc x -> acc ++ [f x])[] xs

-- find the max element by using foldr1
-- foldr1 and foldl1, u don't need to provide them with staring value, it would use the last or 
-- first element as starting value
-- foldr could be used in infinite list, because if you start from right, you could ultimately
-- arrive at the beginning  but foldl not, you start from the first, you may not arrive at the
-- end

maximum_1 :: (Ord a) => [a] -> a
maximum_1 = foldr1 (\x acc -> if x > acc then x else acc)

-- reverse element of a list. by using foldl, start from left, prepend to the empty list

reverse_1 :: [a] -> [a]
reverse_1 xs = foldl(\acc x -> x : acc) [] xs

-- product, product of all ele of a list, using foldr, because foldr could be used
-- for infinite list

product_1 :: (Num a) => [a] -> a
product_1 = foldr1(*)
			
-- filter 
-- usage : filter_1 (>0) [1,-2,3,-4]

filter_1 :: (a -> Bool) -> [a] -> [a]
filter_1 p = foldr(\x acc -> if p x  then x : acc else acc) []
