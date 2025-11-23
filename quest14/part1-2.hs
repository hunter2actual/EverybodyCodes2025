import Data.Array
import Data.Foldable (find)

type Point  = (Int, Int)
type Board = Array Point Bool

parse :: String -> Board
parse file = listArray ((0,0), (length xss - 1, length (head xss) - 1)) $ concat xss
    where
        xss = map (map (=='#')) $ lines file

printBoard :: Board -> IO ()
printBoard board = do
    let ((minY, minX), (maxY, maxX)) = bounds board
    mapM_ (\y -> do
            let row = [ if board ! (y, x) then '#' else '.' 
                      | x <- [minX .. maxX] ]
            putStrLn row
          )
          [minY .. maxY]
    putStrLn ""

-- includes self
numDiagTrues :: Board -> Point -> Int
numDiagTrues board (y, x) = length $ filter id diags
    where
        (maxY, maxX) = snd $ bounds board
        diags = board ! (y, x) :
            [board ! (y', x') |
                   y' <- [y - 1, y + 1], 0 <= y', y' <= maxY,
                   x' <- [x - 1, x + 1], 0 <= x', x' <= maxX]

mapWithIndex :: Ix k => (k -> v -> v) -> Array k v -> Array k v
mapWithIndex f arr = array (bounds arr) [ (i, f i v) | (i, v) <- assocs arr ]

step :: Board -> Board
step board = mapWithIndex go board
    where
        go p x = even (numDiagTrues board p)

countActive :: Board -> Int
countActive board = length $ filter id (elems board)

solve :: Int -> Board -> Int
solve n b = sum $ map countActive (tail $ take (n+1) $ iterate step b)

emptyBoard :: Board
emptyBoard = array ((0,0), (33, 33)) [((y, x), False) | y <- [0..33], x <- [0..33]]

centreMatches :: Board -> Board -> Bool
centreMatches mask board = let
    (maxBoardY, maxBoardX) = snd $ bounds board
    (maxMaskY, maxMaskX) = snd $ bounds mask
    hb = maxBoardY `div` 2
    hm = maxMaskY  `div` 2
    bs = [board ! (y, x) | y <- [hb-hm .. hb+hm+1], x <- [hb-hm .. hb+hm+1]]
    ms = [mask  ! (y, x) | y <- [0..maxMaskY], x <- [0..maxMaskX]]
    in
        and $ zipWith (==) bs ms

main :: IO ()
main = do
    file1 <- readFile "quest14/part1.txt"
    let part1 = parse file1
    print $ solve 10 part1

    file2 <- readFile "quest14/part2.txt"
    let part2 = parse file2
    print $ solve 2025 part2


    -- TOO SLOW for p3, let's get AI to optimise it for us

    -- file3 <- readFile "quest14/part3.txt"
    -- let mask = parse file3
    -- let part3 = emptyBoard
    -- let matched = filter (centreMatches mask) (take 1000000001 $ iterate step part3)
    -- print $ sum $ map countActive matched
