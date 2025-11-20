{-# LANGUAGE BangPatterns #-}

import Data.List (intercalate, find)
import Data.Maybe (fromJust)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S

-- Remove Global State/Memo. Use simple types.
type Visited = S.Set BoardState
type Cell = (Bool, Bool, Bool)  -- (hideout, sheep, dragon)
type BoardState = M.Map (Char, Int) Cell

printBoard :: (Char, Int) -> BoardState -> IO ()
printBoard bounds state = printLines $ toLines bounds state

printLines :: [String] -> IO ()
printLines x = do putStrLn $ intercalate "\n" x ++ "\n"

toLines :: (Char, Int) -> BoardState -> [String]
toLines (maxColumn, maxRow) state = map perRow [1..maxRow]
    where
        perRow y = map (perCell y) ['A'..maxColumn]
        perCell y x = case M.lookup (x, y) state of
            Just cell -> go cell
            Nothing   -> ' ' 
        go (True, True, _) = '8'
        go (True, _, _) = '#'
        go (_, _, True) = 'D'
        go (_, True, _) = 'S'
        go (_, _, _)    = '.'

parse :: String -> ((Char, Int), BoardState)
parse file = ((lastColumn, length ls), state)
    where
        ls = lines file
        parseCell c = (c == '#', c == 'S', c == 'D')
        lastColumn = ['A'..] !! (length (head ls) - 1)
        state = M.fromList
            [((x, y), parseCell c)
            | (y, line) <- zip [1..] ls
            , (x, c)    <- zip ['A'..] line]

toCharCoord :: (Int, Int) -> (Char, Int)
toCharCoord (x, y) = (['A'..'Z'] !! x, y+1)

toIntCoord :: (Char, Int) -> (Int, Int)
toIntCoord (c, y) = (fromEnum c - fromEnum 'A', y-1)

findDragon :: BoardState -> (Char, Int)
findDragon m = fromJust maybeKey
    where
        maybeKey = fst <$> find (isDragon . snd) (M.toList m)
        isDragon (_, _, d) = d

-- **FIXED**: If sheep moves to new square (even off-board Y=5), ADD it to map.
-- This ensures 'noSheep' doesn't trigger true when a sheep escapes.
sheepMove :: BoardState -> (Char, Int) -> BoardState
sheepMove state target@(x, y) = 
    let source = (x, pred y)
        -- Remove sheep from old source
        stateWithoutSheep = M.adjust unsheepify source state
        -- Add sheep to target (inserting if it didn't exist, e.g., Row 5)
        newState = M.alter putSheep target stateWithoutSheep
    in newState
    where
        putSheep Nothing = Just (False, True, False) -- Moving to empty void (Row 5)
        putSheep (Just (h, _, d)) = Just (h, True, d) -- Moving to existing cell
        unsheepify (h, True, d) = (h, False, d)
        unsheepify c = c

dragonMove :: BoardState -> (Char, Int) -> BoardState
dragonMove state (x, y) = M.adjust dragonify (x,y) $ M.adjust undragonify (findDragon state) state
    where
        dragonify   (h, s, False) = (h, s, True)
        undragonify (h, s, True)  = (h, s, False)

-- Checks for captures on the CURRENT board arrangement
eat :: BoardState -> BoardState
eat = M.map go
    where
        go (True, s,    d   ) = (True, s,     d   ) -- hideout protects sheep
        go (h,    True, True) = (h,    False, True) -- sheep is eaten
        go (h,    s,    d   ) = (h,    s,     d   )

dragonMovesOld :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
dragonMovesOld (maxX, maxY) (x, y) =
    [ (i, j)
    | (dx, dy) <- [(1,2), (2,1), (-1,2), (-2,1), (-1,-2), (-2,-1), (1,-2), (2,-1)]
    , let i = x + dx, let j = y + dy
    , i >= 0, j >= 0, i <= maxX, j <= maxY ]

dragonMoves :: (Char, Int) -> BoardState -> [(Char, Int)]
dragonMoves bounds state = map toCharCoord intMoves
    where
        intMoves = dragonMovesOld (toIntCoord bounds) (toIntCoord $ findDragon state)

sheepMoves :: (Char, Int) -> BoardState -> [(Char, Int)]
sheepMoves (maxX, maxY) state = foldl go [] (M.keys state)
    where
        isSheep (h, s, d) = s
        isLegalMove (h, s, d) = not s && (not d || h)
        
        go acc (x, y) = 
            let targetCoord = (x, y+1)
            in acc ++ 
               if isSheep (state ! (x, y)) then 
                   if y == maxY && x <= maxX 
                      then [targetCoord] -- Escape move to Y=5
                      else [(x, y+1)
                           | y < maxY 
                           , M.member targetCoord state
                           , isLegalMove (state ! targetCoord)
                           ]
               else []

toMoveNotation :: Char -> (Char, Int) -> String
toMoveNotation c (x,y) = c:['>', x] ++ show y

-- Win Condition: No sheep left on board
noSheep :: BoardState -> Bool
noSheep = not . any (\(_,(_,s,_)) -> s) . M.toList

-- Loss Condition: Sheep exists at Row > 4
sheepEscaped :: (Char,Int) -> BoardState -> Bool
sheepEscaped (_,maxY) m =
    any (\((_,y),(_,s,_)) -> s && y > maxY) (M.toList m)

-- **REFACTORED SOLVE**: Uses Path-Based Visited set
solve :: (Char,Int) -> BoardState -> [String] -> Visited -> [[String]]
solve bounds board path visited
    -- Cycle Detection: If we've seen this board in THIS path, stop.
    | board `S.member` visited = []

    -- Failure: Sheep escaped (Loss for Dragon)
    | sheepEscaped bounds board = []

    -- Success: All sheep eaten (Win for Dragon)
    | noSheep board = [reverse path]

    -- Recursive Step
    | otherwise = 
        let visited' = S.insert board visited
            possibleSheepMoves = sheepMoves bounds board
        in 
        -- 1. Sheep Move Logic
        if not (null possibleSheepMoves) then
            concat 
            [ let boardSheep = sheepMove board sm in
                concat
                [ let boardDragon = dragonMove boardSheep dm
                      boardFinal = eat boardDragon 
                  in solve bounds boardFinal 
                        (toMoveNotation 'D' dm : toMoveNotation 'S' sm : path) 
                        visited'
                | dm <- dragonMoves bounds boardSheep
                ]
            | sm <- possibleSheepMoves
            ]
        -- 2. Sheep Skip Logic (Dragon takes extra turn)
        else
            concat
            [ let boardDragon = dragonMove board dm
                  boardFinal = eat boardDragon
              in solve bounds boardFinal 
                    (toMoveNotation 'D' dm : path) 
                    visited'
            | dm <- dragonMoves bounds board
            ]

printPath :: [String] -> IO ()
printPath p = do
    putStrLn $ intercalate " " p

main :: IO ()
main = do
    file3 <- readFile "quest10/part3.txt"
    let (bounds, initialBoard) = parse file3
    let printB = printBoard bounds

    printB initialBoard

    putStrLn "Searching for paths (printing as they are found)..."
    
    -- Calculate the list of paths
    let paths = solve bounds initialBoard [] S.empty

    -- Print them one by one as they are computed
    mapM_ printPath paths
    
    putStrLn "Search complete."
    print $ "Found " ++ show (length paths) ++ " paths"