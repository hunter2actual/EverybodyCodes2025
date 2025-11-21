import Data.Array
import Data.Char (digitToInt)
import qualified Data.Set as S
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

type Height = Int
type Point  = (Int, Int)
type HeightMap = Array Point Height

parse :: String -> HeightMap
parse file = listArray ((0,0), (length xss - 1, length (head xss) - 1)) $ concat xss
    where
        xss = map (map digitToInt) $ lines file

neighbours :: HeightMap -> Point -> [Point]
neighbours arr (y, x) = filter (inRange (bounds arr))
    [ (y-1, x)
    , (y+1, x)
    , (y, x-1)
    , (y, x+1)]

downhillReachable :: HeightMap -> S.Set Point -> Point -> S.Set Point
downhillReachable arr excluded start = go S.empty excluded [start]
    where
        go :: S.Set Point -> S.Set Point -> [Point] -> S.Set Point
        go visited exc [] = visited
        go visited exc (p:ps)
            | p `S.member` visited = go visited exc ps
            | otherwise =
                let visited' = S.insert p visited
                    h    = arr!p
                    next = [ q | q <- neighbours arr p, arr!q <= h, not $ q `S.member` exc]
                in go visited' exc (next ++ ps)

printReachable :: HeightMap -> S.Set Point -> IO ()
printReachable arr reachable = do
    let ((minY, minX), (maxY, maxX)) = bounds arr
    mapM_ putStrLn
        [ [ charFor (y, x) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where
        charFor p
            | p `S.member` reachable = head (show (arr!p))
            | otherwise = ' '

bestStart :: HeightMap -> S.Set Point -> Point
bestStart arr excluded = maximumBy (comparing go) (indices arr)
    where
        go p
            | p `S.member` excluded = 0
            | otherwise = length $ downhillReachable arr excluded p

main :: IO ()
main = do
    file1 <- readFile "quest12/part1.txt"
    let part1 = parse file1
    let reachable1 = downhillReachable part1 S.empty (0, 0)
    -- printReachable part1 reachable1
    print $ "P1: Exploded " ++ show (length reachable1) ++ " barrels."
    putStrLn ""

    file2 <- readFile "quest12/part2.txt"
    let part2 = parse file2
    let reachable2 = downhillReachable part2 S.empty (0,0)
    let reachable2' = downhillReachable part2 S.empty (snd $ bounds part2)
    let combined = reachable2 `S.union` reachable2'
    -- printReachable part2 combined
    print $ "P2: Exploded " ++ show (length combined) ++ " barrels."
    putStrLn ""

    file3 <- readFile "quest12/part3.txt"
    let part3 = parse file3
    let start = bestStart part3 S.empty
    let reachable3 = downhillReachable part3 S.empty start

    let start' = bestStart part3 reachable3
    let reachable3' = downhillReachable part3 reachable3 start'

    let exploded = reachable3 `S.union` reachable3'

    let start'' = bestStart part3 exploded
    let reachable3'' = downhillReachable part3 exploded start''

    let exploded' = exploded `S.union` reachable3''
    printReachable part3 exploded'
    print $ "P3: Exploded " ++ show (length exploded') ++ " barrels."
