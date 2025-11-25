import Data.List.Split (splitOn)
import Data.List (elemIndex, sort)

parse :: String -> [Int]
parse file = map read $ splitOn "," file

numBricks :: [Int] -> Int -> Int
numBricks spell wallLength = sum $ map (div wallLength) spell

deriveSpell :: [Int] -> [Int]
deriveSpell wallFragment = sort $ map (+1) $ go [] wallFragment
    where
        go arr xs = case elemIndex 1 xs of
            Just i  -> go (i:arr) $ step (i+1) xs
            Nothing -> arr
        step n ys = zipWith 
            (\i x -> if i `mod` n == 0 then x - 1 else x)
            [1..]
            ys

binarySearch :: (Int -> Int) -> Int -> Int -> Int -> Int
binarySearch f target lo hi =
    let mid = (lo + hi) `div` 2
        v   = f mid
    in
        if (f mid < target) && (f (mid+1) > target)
            then mid -- Case where we have bricks left over
            else
                case compare v target of
                    EQ -> mid
                    LT -> binarySearch f target (mid + 1) hi
                    GT -> binarySearch f target lo (mid - 1)

findUpperBound :: (Int -> Int) -> Int -> Int
findUpperBound f target = go 1
    where
        go hi
            | f hi >= target = hi
            | otherwise      = go (hi * 2)

main :: IO ()
main = do
    file1 <- readFile "quest16/part1.txt"
    print $ numBricks (parse file1) 90

    file2 <- readFile "quest16/part2.txt"
    print $ product $ deriveSpell $ parse file2

    file3 <- readFile "quest16/part3.txt"
    let spell = deriveSpell $ parse file3
    let target = 202520252025000
    let f = numBricks spell
    let upperBound = findUpperBound f target
    print $ binarySearch f target 0 upperBound
