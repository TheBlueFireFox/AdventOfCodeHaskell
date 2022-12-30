module Main ( main ) where

import qualified Data.List as List
import qualified Data.Set as Set

import System.IO ()

splitAt :: Int -> [a] -> ([a], [a])
splitAt x list = let (a, b) = List.splitAt x list in (a, tail b)

-- we know that the delim exists so we can just maybe with 0
splitChunks :: Eq a => a -> [a] -> ([a], [a])
splitChunks delim list =
    maybe ([], []) (\x -> (Main.splitAt x list)) (List.elemIndex delim list)

type Id = (Int, Int)

toId :: String -> Id
toId s = let (l, r) = splitChunks '-' s in (read l, read r)

blocks :: String -> (Id, Id)
blocks s = let (l, r) = splitChunks ',' s in (toId l, toId r)

isOverlapOne :: String -> Bool
isOverlapOne line = let ((l0, l1), (r0, r1)) = blocks line
                    in
                        (l0 <= r0 && r1 <= l1) || (r0 <= l0 && l1 <= r1)

one :: [String] -> Int
one = List.foldl' (\i v -> if isOverlapOne v then i + 1 else i) 0
    . filter (not . null)

isInRange :: Ord a => (a, a) -> (a, a) -> Bool
isInRange (l0, l1) (r0, r1) = (l0 <= r0 && r0 <= l1) || (l0 <= r1 && r1 <= l1)

isOverlapTwo :: String -> Bool
isOverlapTwo line = let (l, r) = blocks line in isInRange l r || isInRange r l

two :: [String] -> Int
two = sum . map (fromEnum . isOverlapTwo) . filter (not . null)

main :: IO ()
main = do
    -- equeals to 
    -- fmap lines (readFile "input.txt")
    -- fmap :: (a -> b) -> f a -> f b
    content <- lines <$> readFile "input.txt"
    print $ one content
    print $ two content
