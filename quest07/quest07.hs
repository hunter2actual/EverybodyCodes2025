import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S

parseRules :: [String] -> Map Char (Char -> Bool)
parseRules raw = Map.fromList [(fst rule, interpret $ snd rule) | rule <- map parseRule raw]

parseRule :: String -> (Char, [Char])
parseRule s = (head s, filter (/= ',') (drop 4 s))

interpret :: [Char] -> (Char -> Bool)
interpret posts = f where f c = c `elem` posts

testName :: Map Char (Char -> Bool) -> String -> Bool
testName rules name = and $ zipWith (rules Map.!) name (tail name)

solveP2 :: Map Char (Char -> Bool) -> [String] -> Int
solveP2 rules names = sum $ zipWith (\i name -> if testName rules name then i else 0) [1..] names

allNames :: Map Char [Char] -> String -> [String]
allNames rules pre
    | length pre >= 11 = [pre]
    | Map.lookup (last pre) rules == Nothing = [pre]
    | otherwise = let
        pres = map (\c -> pre ++ [c]) (rules Map.! last pre)
        in pres ++ foldl (\acc p -> acc ++ allNames rules p) [] pres

solveP3 :: Map Char [Char] -> Map Char (Char -> Bool) -> [String] -> Int
solveP3 rawRules rules names = let
    filteredNames = filter (testName rules) names
    allTheNames = foldl (\acc x -> acc ++ allNames rawRules x) [] filteredNames
    in
        length $ S.fromList $ filter (\x -> length x >= 7) $ allTheNames

main :: IO ()
main = do
    let names1 = ["Nylirin","Nylardith","Nylvyr","Nyirin","Aelvyr","Aelardith","Nyardith","Aelirin","Nyvyr"]
    part1 <- readFile "quest07/part1.txt"
    let rulesP1 = parseRules $ lines part1
    print $ filter (testName rulesP1) names1

    let names2 = ["Vyrlzar","Shaemparth","Zynnylor","Vansyron","Xendphor","Elvarroth","Drazimar","Zynsyron","Zynphor","Xendparth","Elvarnylor","Skarkryth","Rythsyron","Vyrlimar","Skarroth","Xendkryth","Vyrlkryth","Xendimar","Drazparth","Draznylor","Xendzar","Drazkryth","Elvarimar","Vyrlsyron","Elvarzar","Rythimar","Skarnylor","Zynzar","Vyrlnylor","Shaemroth","Shaemnoris","Vannoris","Shaemimar","Zynparth","Skarsyron","Elvarkryth","Rythphor","Zynfyr","Rythparth","Urithphor","Vyrlphor","Drazfyr","Vanroth","Rythnylor","Vannylor","Rythzar","Rythkryth","Elvarfyr","Zynnoris","Vanparth","Vankryth","Zynroth","Xendfyr","Urithkryth","Vyrlnoris","Skarparth","Urithnoris","Shaemzar","Vyrlroth","Urithroth","Elvarnoris","Xendnylor","Elvarphor","Xendroth","Elvarparth","Vyrlfyr","Zynkryth","Shaemnylor","Rythfyr","Vanzar","Rythnoris","Skarfyr","Skarphor","Zynimar","Elvarsyron","Skarnoris","Vanfyr","Vanphor","Vanimar","Shaemfyr","Urithfyr","Skarimar","Vyrlparth","Skarzar","Xendnoris","Draznoris","Urithparth","Drazphor","Drazsyron","Shaemsyron","Drazroth","Urithsyron","Shaemphor","Xendsyron","Drazzar","Urithzar","Shaemkryth","Urithimar","Urithnylor","Rythroth"]
    part2 <- readFile "quest07/part2.txt"
    let rules2 = parseRules $ lines part2
    print $ solveP2 rules2 names2

    let names3 = ["Ny","Nyl","Nyth","Nyss","Nyrix","Rah","Har","Arak","Ryth","Fen","Xan","Ign","Shaem","Ky","Zor","Zyrix","Wyn","Ryss","Ilmar","Luth"]
    part3 <- readFile "quest07/part3.txt"
    let rawRules3 = Map.fromList (map parseRule $ lines part3)
    let rules3 = parseRules $ lines part3
    print $ solveP3 rawRules3 rules3 names3





