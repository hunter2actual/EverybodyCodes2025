-- https://everybody.codes/event/2025/quests/2

import qualified Data.Map as M

data ComplexInt = ComplexInt Int Int deriving Show

infixl 2 ^+
(^+) :: ComplexInt -> ComplexInt -> ComplexInt
ComplexInt x1 y1 ^+ ComplexInt x2 y2 = ComplexInt (x1+x2) (y1+y2)

infixl 3 ^*
(^*) :: ComplexInt -> ComplexInt -> ComplexInt
ComplexInt x1 y1 ^* ComplexInt x2 y2 = ComplexInt (x1*x2 - y1*y2) (x1*y2 + y1*x2)

infixl 4 ^/
(^/) :: ComplexInt -> ComplexInt -> ComplexInt
ComplexInt x1 y1 ^/ ComplexInt x2 y2 = ComplexInt (x1 `quot` x2) (y1 `quot` y2) -- quot rounds towards zero

step :: ComplexInt -> ComplexInt -> ComplexInt
step a start = (start ^* start) ^/ ComplexInt 100000 100000 ^+ a

shouldEngrave :: ComplexInt -> Bool
shouldEngrave a = not $ any outOfBounds $ take 101 $ iterate (step a) (ComplexInt 0 0)

outOfBounds :: ComplexInt -> Bool
outOfBounds (ComplexInt x1 x2) = abs x1 > 1000000 || abs x2 > 1000000

engraving :: ComplexInt -> M.Map (Int, Int) Bool
engraving (ComplexInt x1 x2) =
    M.fromList
        [ ((i, j), shouldEngrave (ComplexInt i j))
        | i <- [x1, x1 + 10 .. x1 + 1000]
        , j <- [x2, x2 + 10 .. x2 + 1000]
        ]

-- ChatGPT for the ASCII preview
renderEngraving :: M.Map (Int, Int) Bool -> IO ()
renderEngraving engraveMap = do
    let keys = M.keys engraveMap
        is = map fst keys
        js = map snd keys
        minI = minimum is
        maxI = maximum is
        minJ = minimum js
        maxJ = maximum js
        -- build each row as a string
        row j = [ if M.findWithDefault False (i, j) engraveMap then 'X' else '.'
                | i <- [minI, minI + 10 .. maxI] ]
    mapM_ putStrLn [ row j | j <- [minJ, minJ + 10 .. maxJ] ]

numTrue :: [Bool] -> Int
numTrue xs = length $ filter (== True) xs

main :: IO ()
main = do
    -- let a = ComplexInt 35300 (-64910) numTrue -> 4076 ✅
    let a = ComplexInt (-4501) 67892
    let engraved = engraving a
    renderEngraving engraved
    print $ numTrue $ M.elems engraved

