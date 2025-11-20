import Data.List (sort)

parse :: String -> [Int]
parse file = map read $ lines file

slice :: Int -> Int -> [a] -> [a]
slice i j xs = drop i (take j xs)

checksum :: [Int] -> Int
checksum xs = sum $ zipWith (*) xs [1..]

phase1 :: Num a => Ord a => [a] -> [a]
phase1 (x:xs) = foldl go [x] xs
    where go acc y = init acc ++ (if last acc > y then [last acc - 1, y + 1] else [last acc, y])

phase1s :: Num a => Ord a => Int -> [a] -> (Int, [a])
phase1s n xs
    | xs == sort xs = (n, xs)
    | otherwise = phase1s (n+1) (phase1 xs)

phase2 :: Num a => Ord a => [a] -> [a]
phase2 (x:xs) = foldl go [x] xs
    where go acc y = init acc ++ (if last acc < y then [last acc + 1, y - 1] else [last acc, y])

phase2s :: Num a => Ord a => Int -> [a] -> (Int, [a])
phase2s n xs
    | n == 10 = (n, xs)
    | and $ zipWith (>=) xs (tail xs) = (n, xs)
    | otherwise = phase2s (n+1) (phase2 xs)

main :: IO ()
main = do
    file1 <- readFile "quest11/part1.txt"
    let ducks1 = parse file1
    let (n, ducks1') = phase1s 0 ducks1
    let (n', ducks1'') = phase2s n ducks1'
    print $ checksum ducks1''
