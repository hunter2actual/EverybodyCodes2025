-- https://everybody.codes/event/2025/quests/2

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

operate :: ComplexInt -> ComplexInt -> ComplexInt
operate a start = (start ^* start) ^/ ComplexInt 10 10 ^+ a

main :: IO ()
main = do
    let result = ComplexInt 0 0
    let a = ComplexInt 154 51
    let operateA = operate a
    print $ operateA.operateA.operateA $ ComplexInt 0 0

