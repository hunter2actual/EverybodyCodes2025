-- https://everybody.codes/event/2025/quests/1

import Data.List

instructions :: [String]
instructions = ["L6","R10","L6","R7","L9","R9","L7","R17","L15","R13","L5","R15","L5","R14","L5","R12","L5","R13","L5","R12","L8","R13","L6","R18","L11","R5","L11","R10","L15"]

names :: [String]
names = ["Harnnoris","Fyrrex","Ryngnar","Ignthyn","Eltvalir","Qyranar","Igngyth","Bryllon","Gavfeth","Elvarfelix","Beldren","Zorverax","Urakxel","Xendlyr","Galloris","Brylnyn","Karthxith","Quorverax","Elaridris","Arkroth"]

toShift :: String -> Int
toShift (pre:num) | pre == 'L' = (-1) * (read num :: Int)
                  | pre == 'R' =   1  * (read num :: Int)

getIndex :: Int -> Int -> [Int] -> Int
getIndex start maxBound steps = 
    (foldl (\acc x -> acc+x) start steps) `mod` maxBound

main :: IO ()
main = do
    let steps = map toShift instructions
    let maxBound = length names
    print $ names !! getIndex 0 maxBound steps
    
