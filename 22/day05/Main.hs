module Main ( main ) where

import qualified Data.Array as Array
import qualified Data.List as List

import System.IO

type Crate = Char

data Move = Move { count :: Int, from :: Int, to :: Int }
    deriving ( Show )

findIndicesAndElementsHelper :: Num t => (a -> Bool) -> t -> [a] -> [(t, a)]
findIndicesAndElementsHelper _ _ [] = []
findIndicesAndElementsHelper p i (c : r) =
    if p c
    then [ (i, c) ] ++ findIndicesAndElementsHelper p (i + 1) r
    else findIndicesAndElementsHelper p (i + 1) r

findIndicesAndElements :: Num t => (a -> Bool) -> [a] -> [(t, a)]
findIndicesAndElements p list = findIndicesAndElementsHelper p 0 list

-- The index that is generated is the internal one,
-- so external ID - 1
mapElementsAndStack :: String -> [(Int, Crate)]
mapElementsAndStack = map (\(i, v) -> ((i + 2) `div` 4, v))
    . findIndicesAndElements (\v -> not (v `elem` [ ' ', '[', ']' ]))

generateMap :: Int -> Int -> [(Int, Crate)] -> [Maybe Crate]
generateMap len index []
    | index < len = [ Nothing ] ++ (generateMap len (index + 1) [])
    | otherwise = []

generateMap len index list@((id, name) : rest)
    | index == id = [ Just name ] ++ (generateMap len (index + 1) rest)
    | index < len = [ Nothing ] ++ (generateMap len (index + 1) list)

stackCount :: Foldable t => t a -> Int
stackCount line = ((List.length line) + 1) `div` 4

-- remove all the 'Nothing', so that the top of the
-- 'stack' is at the head of the list
nothingToEnd :: [Maybe a] -> [a]
nothingToEnd [] = []
nothingToEnd (curr : next) = case curr of
    Just v -> [ v ] ++ (nothingToEnd next)
    Nothing -> (nothingToEnd next)

parseStack :: [[Char]] -> [[Crate]]
parseStack stack =
    let i = (stackCount $ head stack)
    in
        map nothingToEnd . List.transpose . map (generateMap i 0) $
        map mapElementsAndStack stack

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [ [] ]
  where
    f c l@(x : xs)
        | c == delimiter = [] : l
        | otherwise = (c : x) : xs

parseMove :: Foldable t => t Char -> Move
parseMove move =
    let (_ : count : _ : from : _ : to : _) = splitBy ' ' move
    in
        Move { count = (read count), from = (read from), to = (read to) }

parseMoves :: [[Char]] -> [Move]
parseMoves = map parseMove . filter (not . null)

parseCommands :: [[Char]] -> ([[Crate]], [Move])
parseCommands all = let (stack, movements) = span (not . null) all
                        s = parseStack (init stack)
                        m = parseMoves movements
                    in
                        (s, m)

craneProcessMoves :: (a -> b -> a) -> a -> [b] -> a
craneProcessMoves _ crates [] = crates
craneProcessMoves f crates (curr : rest) =
    craneProcessMoves f (f crates curr) rest

doMoveOne :: [[a]] -> Move -> [[a]]
doMoveOne crates move =
    let f = (from move) - 1
        t = (to move) - 1
        crate = head (crates !! f)
    in
        map (\(idx, v) -> (if idx == t
                           then [ crate ] ++ v
                           else if idx == f then tail v else v)) $
        zip [ 0 .. ] crates

craneMoveCratesOne :: [[a]] -> Move -> [[a]]
craneMoveCratesOne crates move
    | (count move) == 0 = crates
    | otherwise = craneMoveCratesOne (doMoveOne crates move)
                                     (Move { count = (count move) - 1
                                           , from  = (from move)
                                           , to    = (to move)
                                           })

craneMoveCratesTwo :: [[a]] -> Move -> [[a]]
craneMoveCratesTwo crates move =
    let c = (count move)
        f = (from move) - 1
        t = (to move) - 1
        (toMove, rest) = splitAt c (crates !! f)
    in
        map (\(idx, v) ->
             (if idx == t then toMove ++ v else if idx == f then rest else v)) $
        zip [ 0 .. ] crates

one, two :: [[Crate]] -> [Move] -> [Crate]
one crates moves = map head $ craneProcessMoves craneMoveCratesOne crates moves
two crates moves = map head $ craneProcessMoves craneMoveCratesTwo crates moves

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let (crates, moves) = parseCommands contents
    print $ one crates moves
    print $ two crates moves
