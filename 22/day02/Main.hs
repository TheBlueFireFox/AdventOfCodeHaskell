import Data.List ()

import System.IO ()

readInput :: IO [String]
readInput = do
    contents <- readFile "input.txt"
    return (lines contents)

data Action = Rock | Paper | Scissors

actionToVal :: Num a => Action -> a
actionToVal Rock = 1
actionToVal Paper = 2
actionToVal Scissors = 3

data State = Win | Draw | Loose

strToState :: Char -> State
strToState a = case a of
    'X' -> Win
    'Y' -> Draw
    'Z' -> Loose

stateToVal :: Num a => State -> a
stateToVal Win = 6
stateToVal Draw = 3
stateToVal Loose = 0

strToAction :: Char -> Action
strToAction a
    | a `elem` [ 'A', 'X' ] = Rock
    | a `elem` [ 'B', 'Y' ] = Paper
    | a `elem` [ 'C', 'Z' ] = Scissors

stateWinning :: Action -> Action -> State
stateWinning a b = case (a, b) of
    (Rock, Scissors) -> Win
    (Paper, Rock) -> Win
    (Scissors, Paper) -> Win
    (Rock, Paper) -> Loose
    (Paper, Scissors) -> Loose
    (Scissors, Rock) -> Loose
    _ -> Draw

stateResultingAction :: State -> Action -> Action
stateResultingAction state action = case (state, action) of
    (Win, Rock) -> Scissors
    (Win, Paper) -> Rock
    (Win, Scissors) -> Paper
    (Loose, Rock) -> Paper
    (Loose, Paper) -> Scissors
    (Loose, Scissors) -> Rock
    (Draw, action) -> action

parseLineOne :: String -> (Action, Action)
parseLineOne line = (strToAction (head line), strToAction (last line))

oneInner :: Num a => String -> a
oneInner line = let (other, me) = parseLineOne line
                in
                    stateToVal (stateWinning me other) + actionToVal me

one :: Num a => [String] -> a
one all = sum (map oneInner all)

twoInner :: Num a => String -> a
twoInner line =
    let (other, state) = (strToAction (head line), strToState (last line))
        me = stateResultingAction state other
    in
        stateToVal (stateWinning me other) + actionToVal me

two :: Num a => [String] -> a
two all = sum (map twoInner all)

main :: IO ()
main = do
    input <- readInput
    let resOne = one input
    let resTwo = two input
    putStrLn ("One: " ++ (show resOne))
    putStrLn ("Two: " ++ (show resTwo))
