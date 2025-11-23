import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import Data.Bits (xor, (.&.), (.|.), complement, shiftL, shiftR, popCount)
import qualified Data.Map.Strict as M

-- A Board is just 34 rows, where each row is a 64-bit integer.
type Board = V.Vector Word64

-- Board Dimensions
rows :: Int
rows = 34

cols :: Int
cols = 34

-- Bitmask to keep rows clean (keep only bottom 34 bits)
rowMask :: Word64
rowMask = (1 `shiftL` cols) - 1

-- | Parse input into a Vector of Word64
parse :: String -> Board
parse str = V.fromList $ map parseRow (lines str)
  where
    parseRow :: String -> Word64
    parseRow chars = foldr (\(i, c) acc -> if c == '#' then acc .|. (1 `shiftL` i) else acc) 0 (zip [0..] chars)

-- | Step the board forward one generation
-- Rule: Active if (Self + Diagonals) count is EVEN.
-- Logic: Even sum is equivalent to NOT (XOR sum).
step :: Board -> Board
step b = V.generate rows genRow
  where
    genRow :: Int -> Word64
    genRow y =
        let
            -- Get current, row above, and row below. Handle boundary with 0.
            curr = b V.! y
            prev = if y == 0 then 0 else b V.! (y - 1)
            next = if y == rows - 1 then 0 else b V.! (y + 1)

            -- Calculate diagonal neighbors using shifts
            -- If index 0 is LSB: Left is (row << 1), Right is (row >> 1)
            -- Neighbors from Row Above (prev)
            ul   = (prev `shiftL` 1)
            ur   = (prev `shiftR` 1)

            -- Neighbors from Row Below (next)
            dl   = (next `shiftL` 1)
            dr   = (next `shiftR` 1)

            -- Self + 4 diagonals
            sumXor = curr `xor` ul `xor` ur `xor` dl `xor` dr

        in
            -- Result is True if sum is Even => NOT (XOR sum)
            complement sumXor .&. rowMask

-- | Count active cells in the board
countActive :: Board -> Int
countActive b = V.sum $ V.map popCount b

-- | Check if the center of the board matches the mask
-- Mask is 8x8. Board is 34x34.
-- Centering logic based on user code:
-- Board Y range: [13..20] (8 rows)
-- Board X range: [13..20] (8 cols)
centreMatches :: Board -> Board -> Bool
centreMatches targetMask board = all rowMatch [0..7]
  where
    -- The mask is 8x8. We only care about the 8 rows in the middle of the board.
    -- Board Center Start Y = 13
    boardStartY = 13
    -- Board Center Start X = 13
    shiftAmount = 13

    rowMatch :: Int -> Bool
    rowMatch i =
        let
            -- Get the relevant row from the board
            bRow = board V.! (boardStartY + i)
            -- Get the pattern from the mask
            mRow = targetMask V.! i
            -- Extract the 8 relevant bits from the board row
            -- We shift right by 13 to align bit 13 to bit 0, then mask 8 bits (0xFF)
            extracted = (bRow `shiftR` shiftAmount) .&. 0xFF
            -- The mask from file is already in bits 0-7
            target    = mRow .&. 0xFF
        in
            extracted == target

-- | Solver State
data Simulation = Sim
    { sStep :: Int
    , sTotalMatches :: Int
    , sBoard :: Board
    }

solve :: Int -> Board -> Board -> Int
solve limit initialBoard mask = go M.empty (Sim 0 0 initialBoard)
  where
    go :: M.Map Board (Int, Int) -> Simulation -> Int
    go history (Sim i total b)
        | i == limit = total
        | otherwise =
            let
                -- Check if current board state matches the mask logic
                matches = centreMatches mask b
                currentActive = if matches then countActive b else 0
                newTotal = total + currentActive

                -- Have we seen this state before?
                stateKey = b
            in
                case M.lookup stateKey history of
                    -- CASE: Cycle Detected!
                    Just (prevStep, prevTotal) ->
                        let
                            cycleLen = i - prevStep
                            remainingSteps = limit - i

                            -- How many full cycles can we skip?
                            cyclesToSkip = remainingSteps `div` cycleLen

                            -- How much score do we gain per cycle?
                            scorePerCycle = total - prevTotal -- (Current total doesn't include current step yet in this logic flow, but 'total' passed to go is accumulation up to i)
                            -- Wait, let's correspond carefully:
                            -- 'total' passed in is sum of steps 0 to i-1.
                            -- 'prevTotal' was sum of steps 0 to prevStep-1.
                            cycleScore = total - prevTotal

                            -- Calculate skipped score
                            skippedScore = cyclesToSkip * cycleScore

                            -- Jump the step counter
                            newStep = i + (cyclesToSkip * cycleLen)
                        in
                            if cyclesToSkip > 0
                            then go M.empty (Sim newStep (total + skippedScore) b) -- Clear map to force simple simulation for remainder
                            else stepForward history i total b -- Cycle too big to fit (rare), just step

                    -- CASE: No Cycle, just step
                    Nothing -> stepForward history i total b

    stepForward hist i total b =
        let
            matches = centreMatches mask b
            added = if matches then countActive b else 0
            nextB = step b
        in
            go (M.insert b (i, total) hist) (Sim (i + 1) (total + added) nextB)

emptyBoard :: Board
emptyBoard = V.replicate rows 0

main :: IO ()
main = do
    -- Load Mask
    file3 <- readFile "quest14/part3.txt"
    let mask = parse file3

    -- Start Simulation
    -- Part 3 starts with empty board
    let start = emptyBoard
    let limit = 1000000000 -- 1 Billion

    let result = solve limit start mask
    print result