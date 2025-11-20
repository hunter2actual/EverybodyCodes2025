import Data.List (sort)

parse :: String -> [Int]
parse file = map read $ lines file

main :: IO ()
main = do
    file3 <- readFile "quest11/part3.txt"
    let ducks = parse file3
    let avg = sum ducks `div` length ducks
    let diffs = map (\x -> x-avg) ducks
    print $ sum $ filter (>0) diffs
