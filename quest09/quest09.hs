import Data.Map (fromList, (!), Map, keys, mapWithKey, elems)
import Data.List
import Data.Graph
import Data.Tree (Tree(rootLabel, subForest))
import Data.Function (on)

parseFile :: String -> Map Int String
parseFile file = fromList . map (\x -> (read (takeWhile (/=':') x) :: Int, tail $ dropWhile (/=':') x)) $ lines file

similarity :: Map Int String -> Int -> Int -> Int
similarity m i j = length $ filter id $ zipWith (==) (m!i) (m!j)

findParents :: Map Int String -> Int -> Maybe (Int, Int)
findParents m i = let
    possibleParents = [ (a,b) | a <- [1 .. length m], b <- [1..a], a /= b, a /= i, b /= i]
    validParents a b child = all (\(x, y, z) -> z==x || z==y) (zip3 a b child)
    in find (\(a,b) -> validParents (m!a) (m!b) (m!i)) possibleParents

findAllParents :: Map Int String -> Map Int (Maybe (Int, Int))
findAllParents m = fromList $ map (\k -> (k, findParents m k)) (keys m)

solve2 :: Map Int String -> Int
solve2 dna = sum $ mapWithKey go parents
    where
        parents = findAllParents dna
        go k (Just (a, b)) = similarity dna a k * similarity dna b k
        go _ Nothing       = 0

makeGraph :: Map Int (Maybe (Int, Int)) -> (Graph, Vertex -> (Int, Int, [Int]), Int -> Maybe Vertex)
makeGraph m = graphFromEdges $ elems $ mapWithKey makeEdge m
    where
        makeEdge k (Just (a,b)) = (k, k, [a,b])
        makeEdge k Nothing      = (k, k, [])

groupFamilies :: (Vertex -> (node, key, [Vertex])) -> Graph -> [[key]]
groupFamilies nodeFromVertex g = map (extract . fmap getKey) (components g)
    where
        getKey v = let (_, key, _) = nodeFromVertex v in key
        extract (Node k forest) = k : concatMap extract forest

main :: IO ()
main = do
    file1 <- readFile "quest09/part1.txt"
    let map1 = parseFile file1
    print $ similarity map1 1 3 * similarity map1 2 3

    file2 <- readFile "quest09/part2.txt"
    let map2 = parseFile file2
    print $ solve2 map2

    file3 <- readFile "quest09/part3.txt"
    let (g,a,b) = makeGraph $ findAllParents $ parseFile file3
    let families = groupFamilies a g -- e.g. [[1,2,4,5],[3,6,7]]
    print $ sum $ maximumBy (compare `on` length) families