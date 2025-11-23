import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import Data.List (sort, nub, foldl')
import Data.Maybe (mapMaybe)

type Point = (Int, Int)
type Vector = (Int, Int)

data Segment = Seg Point Point deriving (Show, Eq)
data Instruction = TurnLeft Int | TurnRight Int deriving Show

turnRight :: Vector -> Vector
turnRight (dx, dy) = (-dy, dx)

turnLeft :: Vector -> Vector
turnLeft (dx, dy) = (dy, -dx)

normSeg :: Segment -> Segment
normSeg (Seg (x1, y1) (x2, y2))
    | x1 < x2 || (x1 == x2 && y1 < y2) = Seg (x1, y1) (x2, y2)
    | otherwise                        = Seg (x2, y2) (x1, y1)

parse :: String -> [Instruction]
parse raw = map parseOne $ splitComma $ filter (/= '\n') raw
    where
        splitComma [] = []
        splitComma s = 
            let (segment, rest) = span (/= ',') s
            in segment : case rest of
                (',':rest') -> splitComma rest'
                _          -> []
                
        parseOne (d:n)
            | d == 'R' = TurnRight (read n)
            | d == 'L' = TurnLeft (read n)
        parseOne _ = error "Invalid format"

buildSegments :: [Instruction] -> ([Segment], Point, Point)
buildSegments instrs = 
    let startPos = (0,0)
        startDir = (0, -1)
        (finalPos, finalDir, segments) = foldl step (startPos, startDir, []) instrs
    in (segments, startPos, finalPos)
    where
        step (currPos, currDir, accSegs) instr =
            let 
                newDir = case instr of
                    TurnRight _ -> turnRight currDir
                    TurnLeft _  -> turnLeft currDir
                dist = case instr of
                    TurnRight n -> n
                    TurnLeft n  -> n
                
                (cx, cy) = currPos
                (dx, dy) = newDir
                nextPos  = (cx + dx * dist, cy + dy * dist)
                newSeg   = normSeg (Seg currPos nextPos)
            in 
                (nextPos, newDir, newSeg : accSegs)

isWallNode :: Point -> [Segment] -> Bool
isWallNode (px, py) segments = any onSegment segments
    where
        onSegment (Seg (sx1, sy1) (sx2, sy2))
            -- Vertical Wall
            | sx1 == sx2 = px == sx1 && py >= sy1 && py <= sy2
            -- Horizontal Wall
            | sy1 == sy2 = py == sy1 && px >= sx1 && px <= sx2
            | otherwise  = False

isPathBlocked :: Segment -> [Segment] -> Bool
isPathBlocked (Seg (x1, y1) (x2, y2)) walls = any covers walls
    where
        path = normSeg (Seg (x1, y1) (x2, y2))
        (px1, py1) = case path of Seg a _ -> a
        (px2, py2) = case path of Seg _ b -> b

        covers (Seg (wx1, wy1) (wx2, wy2))
            -- If Path is Vertical, check against Vertical Walls
            | px1 == px2 && wx1 == wx2 = 
                px1 == wx1 && (max py1 wy1 <= min py2 wy2)
            -- If Path is Horizontal, check against Horizontal Walls
            | py1 == py2 && wy1 == wy2 = 
                py1 == wy1 && (max px1 wx1 <= min px2 wx2)
            | otherwise = False

solveDijkstra :: Point -> Point -> [Segment] -> Maybe Int
solveDijkstra start end walls = 
    let 
        rawX = [x | Seg (x,_) _ <- walls] ++ [x | Seg _ (x,_) <- walls] ++ [fst start, fst end]
        rawY = [y | Seg (_,y) _ <- walls] ++ [y | Seg _ (_,y) <- walls] ++ [snd start, snd end]
        
        expand cs = sort . nub $ concat [[c-1, c, c+1] | c <- cs]
        
        allX = expand rawX
        allY = expand rawY

        xArr = A.listArray (0, length allX - 1) allX
        yArr = A.listArray (0, length allY - 1) allY
        
        xMap = M.fromList $ zip allX [0..]
        yMap = M.fromList $ zip allY [0..]

        startIdx = (xMap M.! fst start, yMap M.! snd start)
        endIdx   = (xMap M.! fst end,   yMap M.! snd end)

        initPQ = S.singleton (0, startIdx)
        initVisited = S.empty

    in search initPQ initVisited xArr yArr walls endIdx end

search :: S.Set (Int, (Int, Int))   
       -> S.Set (Int, Int)          
       -> A.Array Int Int           
       -> A.Array Int Int           
       -> [Segment]                 
       -> (Int, Int)
       -> Point                
       -> Maybe Int
search pq visited xArr yArr walls targetIdx targetPos = 
    case S.minView pq of
        Nothing -> Nothing 
        Just ((cost, currIdx), restPQ)
            | currIdx == targetIdx -> Just cost
            | S.member currIdx visited -> search restPQ visited xArr yArr walls targetIdx targetPos
            | otherwise ->
                let 
                    (cxI, cyI) = currIdx
                    cx = xArr A.! cxI
                    cy = yArr A.! cyI

                    neighborIndices = 
                        [ (cxI + 1, cyI) | cxI + 1 <= snd (A.bounds xArr) ] ++
                        [ (cxI - 1, cyI) | cxI - 1 >= 0 ] ++
                        [ (cxI, cyI + 1) | cyI + 1 <= snd (A.bounds yArr) ] ++
                        [ (cxI, cyI - 1) | cyI - 1 >= 0 ]

                    validMoves = mapMaybe checkNeighbor neighborIndices

                    checkNeighbor (nxI, nyI) = 
                        let 
                            nx = xArr A.! nxI
                            ny = yArr A.! nyI
                            dist = abs (nx - cx) + abs (ny - cy)
                            pathSeg = normSeg (Seg (cx, cy) (nx, ny))
                            destIsWall = (nx, ny) /= targetPos && isWallNode (nx, ny) walls                            
                            pathIsBlocked = isPathBlocked pathSeg walls
                        in 
                            if destIsWall || pathIsBlocked
                            then Nothing 
                            else Just (cost + dist, (nxI, nyI))

                    newPQ = foldl' (\q item -> S.insert item q) restPQ validMoves
                    newVisited = S.insert currIdx visited
                in 
                    search newPQ newVisited xArr yArr walls targetIdx targetPos

main :: IO ()
main = do
    file3 <- readFile "quest15/part3.txt"
    let (segments, start, end) = buildSegments $ parse file3
    print $ solveDijkstra start end segments