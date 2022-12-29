import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

import System.IO

readInput :: IO [String]
readInput = do
    contents <- readFile "input.txt"
    return (lines contents)

mapItem :: Char -> Int
mapItem a
    | 'a' <= a && a <= 'z' = Char.ord a - Char.ord 'a' + 1
    | 'A' <= a && a <= 'Z' = Char.ord a - Char.ord 'A' + 27

findDoubleInSet :: Set.Set Char -> String -> Char
findDoubleInSet _ "" = ' '
findDoubleInSet set (x : str) =
    if x `Set.member` set then x else findDoubleInSet set str

findDoubleEntry :: (String, String) -> Int
findDoubleEntry (l, r) = mapItem $ findDoubleInSet (Set.fromList l) r

-- using integer division to divide it by two and split it up
splitUp :: [String] -> [(String, String)]
splitUp input = [ List.splitAt ((length s) `div` 2) s | s <- input, s /= "" ]

one :: [String] -> Int
one = sum . map findDoubleEntry . splitUp

mappingDoubles :: Set.Set Char -> String -> Set.Set Char
mappingDoubles _ "" = Set.empty
mappingDoubles set (x : str) = if x `Set.member` set
                               then Set.insert x (mappingDoubles set str)
                               else mappingDoubles set str

twoInnerRec :: Set.Set Char -> [String] -> [Char]
twoInnerRec set [] = []
twoInnerRec set [curr] = Set.toList (mappingDoubles set curr)
twoInnerRec set (curr : rest) =
    twoInnerRec (mappingDoubles set curr) rest

twoInner :: [String] -> Int
twoInner b = let ((f : _), r) = splitAt 1 b
                 set = Set.fromList (f)
             in
                 sum $ map mapItem (twoInnerRec set r)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n arr =
    if length arr > n
    then let (left, right) = splitAt n arr in left : chunksOf n right
    else []

two :: [String] -> Int
two = sum . map twoInner . chunksOf 3

main :: IO ()
main = do
    content <- readInput
    print $ one content
    print $ two content
