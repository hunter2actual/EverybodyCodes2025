parse :: String -> [[Int]]
parse file = map go $ lines file
    where
        go l = [start..end]
            where
                start = read (takeWhile (/='-') l) :: Int
                end   = read (tail $ dropWhile (/='-') l) :: Int

makeWheel :: [[Int]] -> [Int]
makeWheel xs = 1 : concat (odds ++ reverse (map reverse evens))
    where
        z = zip [1..] xs
        evens = map snd $ filter (even . fst) z
        odds = map snd $ filter (odd . fst) z

main :: IO ()
main = do
    file2 <- readFile "quest13/part2.txt"
    let part2 = parse file2
    let wheel2 = makeWheel part2
    print $ wheel2 !! (20252025 `mod` length wheel2)

    file3 <- readFile "quest13/part3.txt"
    let part3 = parse file3
    let wheel3 = makeWheel part3
    print $ wheel3 !! (202520252025 `mod` length wheel3)
