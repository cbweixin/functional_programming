{-
4 Problem 24
Lotto: Draw N different random numbers from the set 1..M.

Example:

* (rnd-select 6 49)
(23 1 17 33 21 37)
Example in Haskell:

Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
-}
import System.Random

rnd_select :: Int -> Int -> IO [Int]
rnd_select m n = do 
    gen <- getStdGen
    return $ take m (randomRs (1,n) gen) 

