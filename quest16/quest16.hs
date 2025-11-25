import Data.List.Split (splitOn)
import Data.List (elemIndex)

parse :: String -> [Int]
parse file = map read $ splitOn "," file

numBricks :: [Int] -> Int -> Int
numBricks spell wallLength = sum $ map (div wallLength) spell

deriveSpell :: [Int] -> [Int]
deriveSpell wallFragment = reverse $ map (+1) $ go [] wallFragment
    where
        go arr xs = case elemIndex 1 xs of
            Just i  -> go (i:arr) $ step (i+1) xs
            Nothing -> arr
        step n ys = zipWith 
            (\i x -> if i `mod` n == 0 then x - 1 else x)
            [1..]
            ys

binarySearch :: (Int -> Int) -> Int -> Int -> Int -> Int
binarySearch f target lo hi
    | v == target                           = mid
    | (v < target) && (f (mid+1) > target)  = mid -- this is the case where we have bricks left over
    | v < target                            = binarySearch f target (mid + 1) hi
    | v > target                            = binarySearch f target lo (mid - 1)
    | otherwise                             = error "panic"
    where
        mid = (lo + hi) `div` 2
        v   = f mid

findUpperBound :: (Int -> Int) -> Int -> Int
findUpperBound f target = go 1
    where
        go hi
            | f hi >= target = hi
            | otherwise      = go (hi * 2)

solveP3 :: Int -> [Int] -> Int
solveP3 target wallFragment =
    let
        spell = deriveSpell wallFragment
        f = numBricks spell
        upperBound = findUpperBound f target
    in
        binarySearch f target 0 upperBound

main :: IO ()
main = do
    file1 <- readFile "quest16/part1.txt"
    print $ numBricks (parse file1) 90

    file2 <- readFile "quest16/part2.txt"
    print $ product $ deriveSpell $ parse file2

    file3 <- readFile "quest16/part3.txt"
    print $ solveP3 202520252025000 $ parse file3
