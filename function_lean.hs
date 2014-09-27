-- why const 7 (const 3) = 7
-- how to understand t
--  const :: a->b->a, you input 2 same type variable, you would always get the first one
--  const True False = True, const 7 3 = 7
--  what is (const 3 ) ?  const 3 :: Num a => b -> a. According to my understand,
-- const 3 is a function, its arguments have to be 'Num' ( since 3 is a Num ), it would get
-- another Num and return 3 always
-- what is type of 7 :: Num a->a
-- 7 is a Num, (cont 3 ) is a function, different type ( or same **retur** type ? 
-- since all would return 'Num'?) what is type and return type in haskell? so hard
-- to understand. 
--
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

-- what is type of 'sayMe'? what is return type of 'sayMe'
-- :t sayMe , sayMe :: Integral a => a -> String - a function, argument is 'Integral' type, need one Integral argument and it would return a 'String'
-- :t sayMe 1 , sayMe 1 :: String, return a String
-- go back to the previous example, why const (const 3) 7 doesn't work?
-- all about type, but how to infer
--
