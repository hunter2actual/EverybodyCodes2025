-- https://everybody.codes/event/2025/quests/1

import Data.List

instructions :: [String]
instructions = ["L14","R41","L41","R47","L42","R40","L32","R47","L16","R33","L30","R8","L9","R29","L31","R24","L23","R29","L43","R14","L5","R49","L5","R39","L5","R32","L5","R42","L5","R17","L5","R25","L5","R43","L5","R8","L5","R27","L5","R41","L27","R34","L35","R16","L29","R8","L12","R38","L25","R42","L24","R12","L38","R8","L11","R7","L15","R38","L49"]

names :: [String]
names = ["Tyrdra","Quenaes","Morncoryx","Karthzryn","Syldrith","Belpyros","Brythfroth","Falnyn","Aelkyris","Caloris","Noraksyx","Urakcyth","Ralroth","Breloth","Selknoris","Urgryph","Zalyth","Palddravor","Glynndravor","Thymfarin","Quarnmirix","Nyrixcion","Elarsar","Voraxzyth","Torjor","Brelmyr","Dormyr","Gavagrath","Ilmarardith","Malthys"]

toShift :: String -> Int
toShift (pre:num) | pre == 'L' = (-1) * (read num :: Int)
                  | pre == 'R' =   1  * (read num :: Int)

-- https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
swapTwo :: Int -> Int -> [a] -> [a]
swapTwo f s xs = map snd . foldr (\x a -> 
        if fst x == f then ys !! s : a
        else if fst x == s then ys !! f : a
        else x : a) [] $ ys
    where ys = zip [0..] xs

swapNth :: Int -> [a] -> [a]
swapNth n xs | abs n >= length xs = error "n too large"
             | n < 0 = reverse $ swapTwo 0 (-n) $ reverse xs
             | n >=0 = swapTwo 0 n xs

shuffleList :: [Int] -> [a] -> [a]
shuffleList steps xs = foldl (\acc x -> swapNth (x `mod` length xs) acc) xs steps

main :: IO ()
main = do
    let steps = map toShift instructions
    print $ shuffleList steps names

