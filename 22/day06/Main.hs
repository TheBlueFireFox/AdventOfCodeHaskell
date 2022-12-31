module Main ( main ) where

import qualified Data.Set as Set

one :: (Num t, Ord a) => t -> [a] -> t
one _ [] = -1
one idx str = if Set.size (Set.fromList (take 4 str)) == 4
              then (idx + 4)
              else one (idx + 1) (tail str)

two :: (Num t, Ord a) => t -> [a] -> t
two _ [] = -1
two idx str = if Set.size (Set.fromList (take 14 str)) == 14
              then (idx + 14)
              else two (idx + 1) (tail str)

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    print $ one 0 (contents !! 0)
    print $ two 0 (contents !! 0)
