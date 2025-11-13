import Data.List as L

numCentres :: Int -> [Int] -> Int
numCentres n xs = length $ filter id $ zipWith (\a b -> 2*abs (a-b) == n) xs (tail xs)

intersects :: Ord a => (a, a) -> (a, a) -> Bool
intersects (a1, a2) (b1, b2)
    | a1 == b1 = False
    | a1 == b2 = False
    | a2 == b1 = False
    | a2 == b2 = False
    | otherwise = [sorted!!0, sorted!!2] == L.sort [a1, a2]
               || [sorted!!0, sorted!!2] == L.sort [b1, b2]
        where
            sorted = L.sort [a1, a2, b1, b2]

pairs :: Ord a => [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

knots :: Ord a => [(a, a)] -> Int
knots xs = sum $ zipWith go xs prevs
    where
        prevs = init $ scanl (\acc x -> acc ++ [x]) [] xs
        go cur prev = length $ filter (intersects cur) prev

cuts :: [(Int, Int)] -> (Int, Int) -> Int
cuts xs c = length $ filter (intersects' c) xs

intersects' :: Ord a => (a, a) -> (a, a) -> Bool
intersects' (a1, a2) (b1, b2)
    | a1 == b1 && a2 == b2 = True -- cutting along a thread counts as a cut
    | a1 == b2 && a2 == b1 = True
    | a1 == b1 || a2 == b2 = False
    | a1 == b2 || a2 == b1 = False
    | otherwise = [sorted!!0, sorted!!2] == L.sort [a1, a2]
               || [sorted!!0, sorted!!2] == L.sort [b1, b2]
        where
            sorted = L.sort [a1, a2, b1, b2]

solveP3 :: Int -> [Int] -> Int
solveP3 n xs = maximum $ map (cuts (pairs xs)) (allPossibleCuts n)
    where allPossibleCuts n = [ (a,b) | a <- [1..n], b <- [1..n], a /= b]

main :: IO ()
main = do
    part1 <- readFile "quest08/part1.txt"
    let sequence1 = (read ("[" ++ part1 ++ "]") :: [Int])
    print $ numCentres 32 sequence1

    part2 <- readFile "quest08/part2.txt"
    let sequence2 = (read ("[" ++ part2 ++ "]") :: [Int])
    print $ knots . pairs $ sequence2

    part3 <- readFile "quest08/part3.txt"
    let sequence3 = (read ("[" ++ part3 ++ "]") :: [Int])
    print $ solveP3 256 sequence3 -- Brute force ~9 minutes

