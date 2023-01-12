module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Debug.Trace as Trace

data Coord = Coord Int Int
    deriving ( Show, Eq, Ord )

data Direction = N | W | E | S
    deriving ( Show, Eq, Ord )

data CoordHeight = CoordHeight Coord Direction
    deriving ( Show, Eq, Ord )

type Grid = Map.Map Coord Int

debug :: String -> a -> a
debug = Trace.trace

makeGridInner :: Int -> [Char] -> [(Coord, Int)]
makeGridInner idx = map (\(idy, v) -> (Coord idx idy, v)) . zip [ 0 .. ]
    . map Char.digitToInt

makeGrid :: [[Char]] -> Grid
makeGrid = Map.fromList . concat . map (uncurry makeGridInner) . zip [ 0 .. ]

gridSize :: Map.Map k a -> Int
gridSize = fromIntegral . floor . sqrt . fromIntegral . Map.size

gridPrinter :: Show a => Map.Map Coord a -> String
gridPrinter m =
    let
        helper x =
            unwords [ show (m Map.! Coord x y) | y <- [ 0 .. gridSize m - 1 ] ]
    in 
        unlines [ helper x | x <- [ 0 .. gridSize m - 1 ] ]

gridPrintHelperHeight
    :: Show a => Int -> Map.Map CoordHeight a -> Direction -> Int -> String
gridPrintHelperHeight x m d s = unwords $ map show $
    [ m Map.! CoordHeight (Coord x y) d | y <- [ 0 .. s ] ]

gridPrinterHeight
    :: Show a => Direction -> Map.Map CoordHeight a -> Int -> String
gridPrinterHeight d m s =
    unlines [ gridPrintHelperHeight x m d s | x <- [ 0 .. s ] ]

generateHeightMapInner :: Map.Map Coord Int
                       -> Direction
                       -> (Int -> [Coord])
                       -> [(CoordHeight, Int)]
generateHeightMapInner grid dir genCoord =
    let
        accum a v = (max a v, max a v)
        generator y = [ grid Map.! c | c <- genCoord y ]
        nMap y = snd $ List.mapAccumL accum (minBound :: Int) $ generator y
        gen y = map (\c -> CoordHeight c dir) (genCoord y)
        mapping y = zip (gen y) (nMap y)
    in 
        concat $ map mapping $ [ 0 .. gridSize grid - 1 ]

generateHeightMap :: Grid -> Map.Map CoordHeight Int
generateHeightMap grid =
    let
        iter = [ 0 .. gridSize grid - 1 ]
        riter = reverse [ 0 .. gridSize grid - 1 ]
        n = generateHeightMapInner grid N (\y -> [ Coord x y | x <- iter ])
        s = generateHeightMapInner grid S (\y -> [ Coord x y | x <- riter ])
        w = generateHeightMapInner grid W (\x -> [ Coord x y | y <- iter ])
        e = generateHeightMapInner grid E (\x -> [ Coord x y | y <- riter ])
    in 
        Map.fromList $ n ++ s ++ w ++ e

genNorth :: Coord -> CoordHeight
genNorth (Coord x y) = CoordHeight (Coord (x - 1) y) N

genSouth :: Coord -> CoordHeight
genSouth (Coord x y) = CoordHeight (Coord (x + 1) y) S

genWest :: Coord -> CoordHeight
genWest (Coord x y) = CoordHeight (Coord x (y - 1)) W

genEast :: Coord -> CoordHeight
genEast (Coord x y) = CoordHeight (Coord x (y + 1)) E

one :: Grid -> Int
one grid =
    let
        hMap = generateHeightMap grid
        genDir c = [ f c | f <- [ genNorth, genSouth, genWest, genEast ] ]
        genMDir (x, y) = [ hMap Map.! v | v <- genDir (Coord x y) ]

        isVisible (Coord x y) = fromEnum $
            List.any (< grid Map.! Coord x y) (genMDir (x, y))

        countRow x =
            sum $ map isVisible [ Coord x y | y <- [ 1 .. gridSize grid - 2 ] ]

        summed = sum $ map countRow [ 1 .. gridSize grid - 2 ]
    in 
        summed + (gridSize grid) * 4 - 4

main :: IO ()
main = do
    contents <- lines <$> readFile "input.ex.txt"
    let m = makeGrid contents
    putStrLn $ "one: " ++ show (one m) ++ "\n"
    
    putStrLn $ gridPrinter m
    let mm = generateHeightMap m
    putStrLn "N -> S"
    putStrLn $ gridPrinterHeight N mm (gridSize m - 1)
    putStrLn "E -> W"
    putStrLn $ gridPrinterHeight E mm (gridSize m - 1)
    putStrLn "S -> N"
    putStrLn $ gridPrinterHeight S mm (gridSize m - 1)
    putStrLn "W -> E"
    putStrLn $ gridPrinterHeight W mm (gridSize m - 1)
