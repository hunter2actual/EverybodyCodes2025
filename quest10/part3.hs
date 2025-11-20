{-# LANGUAGE BangPatterns #-}

import Data.Bits
import Data.Word (Word64)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

-- -----------------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------------

-- Key: (SheepMask, DragonIndex)
-- Value: 
--   Nothing = Currently Visiting (Cycle detected -> 0 paths from here)
--   Just n  = Computed Count (n winning paths from this state)
type GameKey = (Word64, Int)
type Memo = M.Map GameKey (Maybe Int)

data GameConfig = Config
    { cWidth    :: !Int
    , cHeight   :: !Int
    , cHideouts :: !Word64
    }

type Solver a = State Memo a

-- -----------------------------------------------------------------------------
-- Bit / Index Helpers
-- -----------------------------------------------------------------------------

toIdx :: Int -> Int -> Int -> Int
toIdx w x y = y * w + x

fromIdx :: Int -> Int -> (Int, Int)
fromIdx w i = (i `rem` w, i `quot` w)

-- -----------------------------------------------------------------------------
-- Logic
-- -----------------------------------------------------------------------------

parse :: String -> (GameConfig, GameKey)
parse str = (config, (sheep, dragon))
  where
    ls = lines str
    h = length ls
    w = length (head ls)
    cells = [ (toIdx w x y, c) 
            | (y, line) <- zip [0..] ls
            , (x, c) <- zip [0..] line 
            ]
    sheep = foldl (\acc (i, c) -> if c == 'S' then setBit acc i else acc) 0 cells
    hideouts = foldl (\acc (i, c) -> if c == '#' then setBit acc i else acc) 0 cells
    dragon = fst $ head $ filter (\(_, c) -> c == 'D') cells
    config = Config w h hideouts

getDragonMoves :: GameConfig -> Int -> [Int]
getDragonMoves cfg idx = 
    [ nIdx
    | let (x, y) = fromIdx (cWidth cfg) idx
    , (dx, dy) <- [(1,2), (2,1), (-1,2), (-2,1), (-1,-2), (-2,-1), (1,-2), (2,-1)]
    , let nx = x + dx
    , let ny = y + dy
    , nx >= 0, nx < cWidth cfg, ny >= 0, ny < cHeight cfg
    , let nIdx = toIdx (cWidth cfg) nx ny
    ]

-- Returns: (SourceIndex, TargetIndex, IsEscape)
getSheepMoves :: GameConfig -> Word64 -> Int -> [(Int, Int, Bool)]
getSheepMoves cfg sheepMask dragonIdx = 
    [ (idx, targetIdx, isEscape)
    | idx <- [0 .. (cWidth cfg * cHeight cfg) - 1]
    , testBit sheepMask idx 
    , let (x, y) = fromIdx (cWidth cfg) idx
    , let targetY = y + 1
    , let targetIdx = toIdx (cWidth cfg) x targetY
    , let isEscape = targetY >= cHeight cfg
    , isEscape || (
         let hasSheep = testBit sheepMask targetIdx
             isHideout = testBit (cHideouts cfg) targetIdx
             isDragon = targetIdx == dragonIdx
         in not hasSheep && (not isDragon || isHideout)
      )
    ]

eat :: GameConfig -> Word64 -> Int -> Word64
eat cfg sheepMask dIdx =
    if testBit (cHideouts cfg) dIdx
        then sheepMask 
        else clearBit sheepMask dIdx 

-- -----------------------------------------------------------------------------
-- Solver
-- -----------------------------------------------------------------------------

solve :: GameConfig -> Word64 -> Int -> Solver Int
solve cfg sheep dragon = do
    memo <- get
    let key = (sheep, dragon)
    
    case M.lookup key memo of
        -- Cycle detected: In a counting problem, loops don't contribute to finite winning paths.
        Just Nothing -> return 0 
        -- Already computed
        Just (Just count) -> return count
        -- New state
        Nothing -> compute key
        
  where
    compute :: GameKey -> Solver Int
    compute key = do
        -- Mark as visiting (to detect cycles)
        modify' (M.insert key Nothing)
        
        let (curSheep, curDragon) = key
        
        count <- if curSheep == 0 
               then return 1 -- WIN: Found 1 valid path (the path that led here)
               else do
                   let sMoves = getSheepMoves cfg curSheep curDragon
                   
                   if null sMoves
                       then do
                           -- SKIP TURN logic: Sheep can't move, Dragon moves again
                           let dMoves = getDragonMoves cfg curDragon
                           -- Sum counts from all recursive branches
                           sum <$> mapM (\dIdx -> do
                                            let newSheep = eat cfg curSheep dIdx
                                            solve cfg newSheep dIdx
                                        ) dMoves
                       else do
                           -- NORMAL TURN: Sheep moves, then Dragon moves
                           -- We sum up all successful paths found in children
                           sum <$> mapM (\(sSrc, sDst, isEscape) -> do
                                let sheepAfterMove = if isEscape 
                                                     then clearBit curSheep sSrc
                                                     else setBit (clearBit curSheep sSrc) sDst
                                
                                if isEscape 
                                    then return 0 -- LOSS: Sheep escaped
                                    else do
                                        let dMoves = getDragonMoves cfg curDragon
                                        sum <$> mapM (\dIdx -> do
                                            let finalSheep = eat cfg sheepAfterMove dIdx
                                            solve cfg finalSheep dIdx
                                            ) dMoves
                                ) sMoves

        -- Store result strictly
        modify' (M.insert key (Just count))
        return count

-- -----------------------------------------------------------------------------
-- Main
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
    input <- readFile "quest10/part3.txt"
            
    let (cfg, (initialSheep, initialDragon)) = parse input
    
    putStrLn $ "Board Size: " ++ show (cWidth cfg) ++ "x" ++ show (cHeight cfg)
    putStrLn "Computing number of solutions..."
    
    -- Use evalState to discard the final map and just get the result
    let count = evalState (solve cfg initialSheep initialDragon) M.empty
    
    putStrLn $ "Total unique winning sequences: " ++ show count