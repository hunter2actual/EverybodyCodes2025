{-# LANGUAGE BangPatterns #-}

import Data.List (intercalate, mapAccumL)
import Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Set as S

extract :: Char -> [String] -> [String]
extract c = map $ map (\x -> if x == c then c else '.')

printBoard :: [String] -> IO ()
printBoard x = do putStrLn $ intercalate "\n" x ++ "\n"

dragonMoves :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
dragonMoves (maxX, maxY) (x, y) =
    [ (i, j)
    | (dx, dy) <- [(1,2), (2,1), (-1,2), (-2,1), (-1,-2), (-2,-1), (1,-2), (2,-1)]
    , let i = x + dx, let j = y + dy
    , i >= 0, j >= 0, i <= maxX, j <= maxY ]

nthDragonMove :: Int -> (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
nthDragonMove n bounds pos = iterate step (S.singleton pos) !! n
    where
        step ps = S.fromList [ m | p <- S.toList ps, m <- dragonMoves bounds p ]

nthDragonGrid :: Int -> (Int, Int) -> (Int, Int) -> [String]
nthDragonGrid n bounds pos = toStrings bounds $ S.toList $ nthDragonMove n bounds pos

toStrings :: (Int, Int) -> [(Int, Int)] -> [String]
toStrings (bx, by) points = grid
    where
        ps = S.fromList points
        xs = map fst points
        ys = map snd points
        row y = [if S.member (x,y) ps then 'X' else '.' | x <- [0..max (bx-1) $ maximum xs]]
        grid = [row y | y <- [0..max (by-1) $ maximum ys]]

marchSheep :: [String] -> [String]
marchSheep sheeps = nowt : init sheeps
    where nowt = replicate (length $ head sheeps) '.'

feast :: [String] -> [String] -> [String] -> (Int, [String])
feast hss sss dss = combine $ map go $ zip3 hss sss dss
    where go (hs,ss,ds) = map eat $ zip3 hs ss ds

eat :: (Char, Char, Char) -> (Bool, Char)
eat (h, s, d) = (s=='S' && d=='X' && h=='.'           -- was the sheep eaten
                ,if d=='X' && h=='.' then '.' else s) -- new sheep state

combine :: [[(Bool, Char)]] -> (Int, [String])
combine xss = (trues, strings)
    where
        trues   = length [ () | (b, _) <- concat xss, b ]
        strings = [ [ c | (_, c) <- xs ] | xs <- xss ]

doMove :: [String] -> [String] -> [String] -> (Int, [String])
doMove hss sss dss = 
    let
        (eaten1, remSheep1) = feast hss sss dss
        marched = marchSheep remSheep1
        (eaten2, remSheep2) = feast hss marched dss
    in  (eaten1 + eaten2, remSheep2)

simulate :: [String] -> [String] -> [[String]] -> (Int, [String])
simulate hideouts = go 0
    where
        go !eatenSoFar !sheep [] = (eatenSoFar, sheep)
        go !eatenSoFar !sheep (dragon:rest) =
            let (eaten, !sheep') = doMove hideouts sheep dragon
                eatenSoFar' = eatenSoFar + eaten
            in go eatenSoFar' sheep' rest

main :: IO ()
main = do
    file2 <- readFile "quest10/part2.txt"
    let part2 = lines file2
    let hideouts = extract '#' part2
    let sheep = extract 'S' part2
    let (bx, by) = (length $ head part2, length part2)
    let centre = (bx `div` 2, by `div` 2)
    let dragons = map (\x -> nthDragonGrid x (bx, by) centre) [1..20]
    let (totalEaten, lastSheep) = simulate hideouts sheep dragons
    print totalEaten
