-- https://everybody.codes/event/2025/quests/1

import Data.List

instructions :: [String]
instructions = ["L9","R5","L2","R6","L2","R2","L2","R9","L6","R8","L9"]

names :: [String]
names = ["Jalzral","Nyrixox","Ozanbryn","Kazfyr","Qalzryn","Brynmir","Zraalgryph","Aeleth","Quarnkryth","Aeorkyr"]

toShift :: String -> Int
toShift (pre:num) | pre == 'L' = (-1) * (read num :: Int)
                  | pre == 'R' =   1  * (read num :: Int)

getIndex :: Int -> Int -> Int -> [Int] -> Int
getIndex start minBound maxBound steps = 
    foldl (\acc x -> clamp minBound maxBound acc+x) start steps

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

main :: IO ()
main = do
    let steps = map toShift instructions
    let maxBound = length names - 1
    print $ names !! getIndex 0 0 maxBound steps
    
