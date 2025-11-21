parse :: String -> [Int]
parse file = map read $ lines file

makeWheel :: [Int] -> [Int]
makeWheel xs = reverse evens ++ [1] ++ odds
    where
        z = zip [1..] xs
        evens = map snd $ filter (even . fst) z
        odds = map snd $ filter (odd . fst) z

startingIndex :: [Int] -> Int
startingIndex xs
    | even $ length xs = length xs `div` 2 - 1
    | odd  $ length xs = length xs `div` 2

main :: IO ()
main = do
    file1 <- readFile "quest13/part1.txt"
    let part1 = parse file1
    print part1
    let wheel = makeWheel part1
    let idx = (startingIndex wheel + 2025) `mod` length wheel
    print $ wheel !! idx


