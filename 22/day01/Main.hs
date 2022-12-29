import System.IO ()
import Data.List

readInput :: IO [String]
readInput = do 
    contents <- readFile "input.txt"
    return (lines contents)

transmutInput :: [String] -> [[String]]
transmutInput [] = []
transmutInput input = let (a, b) = span (\x -> x /= "") input in 
        -- remove first unneded value
        a : (transmutInput(dropWhile (\x -> x == "") b))

parseInput :: [String] -> [[Int]]
parseInput input = map (\x -> map read x) (transmutInput input)



one :: [String] -> Int
one input = maximum $ map sum (parseInput input)

two :: [String] -> Int
two input = sum $ take 3 (reverse $ sort (map sum (parseInput input)))

main :: IO ()
main = do
    putStr "Result " 
    input <- readInput
    let resOne = one input
    let resTwo = two input
    print resOne
    print resTwo
