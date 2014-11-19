--import System.Random(getStdGen)
--import System.Random(randomRs)
{-
3 Problem 23
Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)
Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}
import System.Random
import Data.List(nub)

rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n 
  | n < 0 = error "N must be greater than zero"
  | otherwise = do gen <- getStdGen 
                   return $ take n $ nub $ [xs !! x | x <- randomRs (0, (length xs) -1) gen] 


