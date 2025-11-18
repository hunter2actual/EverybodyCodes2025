import Data.Array
import Data.List (nub, intercalate)
import Data.Bifunctor
import qualified Data.Set as Set

parse :: String -> Array (Int, Int) Char
parse file = array ((0,0), (maxY, maxX)) [((y, x), (ls!!y)!!x) | x <- [0..maxY], y <- [0..maxX]]
    where
        ls = lines file
        maxX = length (ls!!1) - 1
        maxY = length ls - 1

knightMoves :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
knightMoves (maxX, maxY) (x, y) = [(i, j)
        | (i, j) <- map (bimap (x+) (y+)) possMoves
        , i >= 0 && j >= 0 && i <= maxX && j <= maxY]
    where
        possMoves = [(1,2), (2,1), (-1,2), (-2,1), (-1,-2), (-2,-1), (1,-2), (2,-1)]

nKnightMoves :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
nKnightMoves 1 bounds pos = knightMoves bounds pos
nKnightMoves n bounds pos = let x = knightMoves bounds pos
    in nub $ concatMap (nKnightMoves (n-1) bounds) x ++ x

printGrid :: [(Int, Int)] -> IO ()
printGrid points = do
    let ps = Set.fromList points
        xs = map fst points
        ys = map snd points
        row y = [if Set.member (x,y) ps then 'X' else '.' | x <- [0..maximum xs]]
        grid = [row y | y <- [0..maximum ys]]
    putStrLn $ intercalate "\n" grid

numEatenSheep :: Array (Int, Int) Char -> [(Int, Int)] -> Int
numEatenSheep arr ps = length $ filter id $ map (\(x,y) -> arr!(y,x) == 'S') ps

main :: IO ()
main = do
    file1 <- readFile "quest10/part1.txt"
    let part1 = parse file1
    let (bx, by) = snd $ bounds part1
    let dragon = (bx `div` 2, by `div `2)
    let eaten = nKnightMoves 4 (bx, by) dragon
    print $ numEatenSheep part1 eaten
