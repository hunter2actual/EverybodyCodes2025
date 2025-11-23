import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), Seq(Empty), (><))
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Vector = (Int, Int)

data Instruction = TurnLeft Int | TurnRight Int
    deriving Show


turnRight :: Vector -> Vector
turnRight (dx, dy) = (-dy, dx)

turnLeft :: Vector -> Vector
turnLeft (dx, dy) = (dy, -dx)

parse :: String -> [Instruction]
parse raw = map go $ splitOn "," raw
    where
        go (d:n)
            | d == 'R' = TurnRight (read n)
            | d == 'L' = TurnLeft (read n)
        go _ = error "Invalid format"

buildMap :: [Instruction] -> (S.Set Point, Point)
buildMap instrs = go instrs (0,0) (0,-1) S.empty
    where
        go [] currPos _ wallSet = (wallSet, currPos)
        go (i:is) currPos currDir wallSet =
            let 
                newDir = case i of
                    TurnRight _ -> turnRight currDir
                    TurnLeft _  -> turnLeft currDir
                
                dist = case i of
                    TurnRight n -> n
                    TurnLeft n  -> n

                (endPos, newPoints) = generateSegment currPos newDir dist
                newWallSet = S.union wallSet (S.fromList newPoints)
            in go is endPos newDir newWallSet

generateSegment :: Point -> Vector -> Int -> (Point, [Point])
generateSegment (x,y) (dx,dy) d = 
    let points = [ (x + dx*k, y + dy*k) | k <- [1..d] ]
        final  = last points
    in (final, points)

solveBFS :: Point -> Point -> S.Set Point -> Maybe Int
solveBFS start end walls = bfs (Seq.singleton (start, 0)) (S.singleton start)
    where
        bfs Empty _ = Nothing 
        bfs ((curr, dist) :<| rest) visited
            | curr == end = Just dist
            | otherwise = 
                let 
                    (cx, cy) = curr
                    -- Up, Down, Left, Right
                    neighbors = [(cx+1, cy), (cx-1, cy), (cx, cy+1), (cx, cy-1)]
                    
                    isValid p = 
                        let isWall   = S.member p walls
                            isSeen   = S.member p visited
                        in not isSeen && (not isWall || p == end)
                    
                    validNeighbors = filter isValid neighbors
                    newQueue = rest >< Seq.fromList [(n, dist + 1) | n <- validNeighbors]
                    newVisited = S.union visited (S.fromList validNeighbors)
                in 
                    bfs newQueue newVisited

main :: IO ()
main = do
    file1 <- readFile "quest15/part1.txt"
    let (walls1, goal1) = buildMap $ parse file1
    print $ solveBFS (0,0) goal1 walls1
    
    file2 <- readFile "quest15/part2.txt"
    let (walls2, goal2) = buildMap $ parse file2
    print $ solveBFS (0,0) goal2 walls2