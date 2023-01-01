module Main where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Debug.Trace as Trace

debug = Trace.trace

type Path = [String]

-- current Tree, Position, Rest of Instructions
type State = (Tree, Path, [String])

data Tree = File { name :: String, size :: Int }
          | Directory { name :: String, children :: [Tree] }
    deriving ( Show )

defaultDirectory :: Tree
defaultDirectory = Directory { name = "/", children = [] }

newDirectory :: String -> [Tree] -> Tree
newDirectory name children = Directory { name = name, children = children }

runInTree :: (Tree -> Tree) -> Tree -> Path -> Path -> Tree
runInTree onFound elem _ [ curr ]
    | (name elem) == curr = onFound elem
    | otherwise = elem
runInTree onFound elem fullPath (curr : path)
    | (name elem) == curr = case elem of
        File{} -> error ("could found a file instead of a directory <"
                         ++ (show fullPath) ++ ">")
        Directory{name = n, children = c} ->
            newDirectory n (map (\x -> runInTree onFound x fullPath path) c)
    | otherwise = elem

insertAtHelper :: Tree -> Tree -> Tree
insertAtHelper toInsert elem = case elem of
    File{} -> error ("cannot insert < " ++ (name toInsert) ++ "> on a file <"
                     ++ (name elem) ++ ">")
    Directory{} -> toInsert

insertAt :: Tree -> Tree -> [String] -> Tree
insertAt tree node path = runInTree (insertAtHelper node) tree path path

printTreeHelper :: Tree -> Int -> String
printTreeHelper tree indent = case tree of
    File{name = n, size = s} -> (List.replicate indent ' ' ++ "- " ++ n
                                 ++ " (file, size=" ++ show s ++ ")\n")
    Directory{name = n, children = c} ->
        (List.replicate indent ' ' ++ "- " ++ n ++ " (dir)\n")
        ++ (List.foldl (++) "" (map (\x -> printTreeHelper x (indent + 2)) c))

printTree :: Tree -> String
printTree tree = printTreeHelper tree 0

-- '$ cd '/ -> 5
lengthCd :: Int
lengthCd = 5

-- $ cd / -> /
parseCd :: State -> State
parseCd (tree, path, (curr : rest)) =
    let
        name = drop lengthCd curr
    in 
        case name of
            "/" -> (tree, [ "/" ], rest)
            ".." -> (tree, init path, rest)
            p -> parseLines (tree, (path ++ [ name ]), rest)

parseLsOutInner :: [String] -> Tree
parseLsOutInner (is : name : _)
    | "dir" == is = Directory { name = name, children = [] }
    | otherwise = File { name = name, size = (read is) }

parseLsOut :: [String] -> [Tree]
parseLsOut = map (parseLsOutInner . words)

-- $ ls 
-- \d+ \w+
-- dir \w+
parseLs :: State -> State
parseLs (tree, workDir, (_ : rest)) =
    let
        (children, todo) = span (\x -> (head x) /= '$') rest
        newDir = newDirectory (last workDir) (parseLsOut children)
    in 
        (insertAt tree newDir workDir, workDir, todo)

-- '$ ls'
lengthLs :: Int
lengthLs = 4

-- $ cd /
-- $ ls
parseLine :: State -> State
parseLine state@(_, _, (line : _))
    | "$ cd" == com = parseCd state
    | "$ ls" == com = parseLs state
  where
    com = take lengthLs line

parseLines :: State -> State
parseLines state@(_, _, []) = state
parseLines state@(_, _, [ "" ]) = state
parseLines state = parseLines $ parseLine state

parse :: [[Char]] -> Tree
parse lines = let
                  (tree, _, _) = parseLines (defaultDirectory, [ "/" ], lines)
              in 
                  tree

data Type = F | D
    deriving ( Show, Eq )

mapTreeHandler :: [String]
               -> (Map.Map [String] (Type, Int), Int)
               -> Tree
               -> (Map.Map [String] (Type, Int), Int)
mapTreeHandler path (values, s) node =
    let
        (v, sNew) = mapTree path node values
    in 
        (v, sNew + s)

mapTree :: [String]
        -> Tree
        -> Map.Map [String] (Type, Int)
        -> (Map.Map [String] (Type, Int), Int)
mapTree path node values = case node of
    File{name = name, size = size} ->
        (Map.insert (path ++ [ name ]) (F, size) values, size)
    Directory{name = name, children = children} ->
        let
            pathNew = (path ++ [ name ])
            (valuesNew, size) =
                List.foldl' (mapTreeHandler pathNew) (values, 0) children
        in 
            (Map.insert pathNew (D, size) valuesNew, size)

maxInterestingDirs :: Int
maxInterestingDirs = 100000

oneHelper sizeTot (t, s) = case t of
    F -> sizeTot
    D -> if s <= maxInterestingDirs then sizeTot + s else sizeTot

one :: Tree -> Int
one tree = Map.foldl oneHelper 0 $ fst $ mapTree [] tree Map.empty

spaceTotal :: Int
spaceTotal = 70000000

spaceForUpdate :: Int
spaceForUpdate = 30000000

two :: Tree -> Int
two tree = let
               m = fst $ mapTree [] tree Map.empty
               used = snd $ m Map.! [ "/" ]
               needed = (spaceTotal - spaceForUpdate - used) * (-1)
           in 
               minimum $ map snd $ filter (\(t, v) -> t == D && needed <= v) $
               map snd $ Map.toList m

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let tree = parse (filter (not . null) contents)
    putStrLn $ printTree tree
    print $ one tree
    print $ two tree
