import Data.Maybe (isNothing, fromMaybe)
import Data.List
import Data.Ord (comparing)

data Sword a = EmptyNode | Node (Maybe a) a (Maybe a) (Sword a) deriving (Show)

swordInsert :: (Ord a) => a -> Sword a -> Sword a
swordInsert x EmptyNode = Node Nothing x Nothing EmptyNode
swordInsert x (Node left middle right next)
    | (x < middle) && isNothing left = Node (Just x) middle right next
    | (x > middle) && isNothing right = Node left middle (Just x) next
    | otherwise = Node left middle right (swordInsert x next)

buildSword :: [Int] -> Sword Int
buildSword = foldl (flip swordInsert) EmptyNode

parseSword :: String -> [Int]
parseSword x = map read $ words $ replace (`elem` ":,") ' ' x
    where replace pred b = map (\x -> if pred x then b else x)

valueOfSword :: (Num a, Show a) => Sword a -> Int
valueOfSword = read . go
    where go EmptyNode = ""
          go (Node _ m _ next) = show m ++ go next

compareSwords :: (Num a, Ord a, Show a) => (Int, Sword a) -> (Int, Sword a) -> Ordering
compareSwords (id1, s1) (id2, s2) =
    comparing valueOfSword s1 s2 <> tieBreakSwords s1 s2 <> compare id1 id2

tieBreakSwords :: (Num a, Ord a) => Sword a -> Sword a -> Ordering
tieBreakSwords EmptyNode EmptyNode = EQ
tieBreakSwords _         EmptyNode = GT
tieBreakSwords EmptyNode _         = LT
tieBreakSwords (Node l1 m1 r1 nx1) (Node l2 m2 r2 nx2) =
    compare sum1 sum2 <> tieBreakSwords nx1 nx2
    where
        sum1 = fromMaybe 0 l1 + m1 + fromMaybe 0 r1
        sum2 = fromMaybe 0 l2 + m2 + fromMaybe 0 r2

main :: IO ()
main = do
    print "P1"
    let sword1 = "10:8,7,3,4,7,9,1,4,6,3,8,9,8,9,5,3,5,8,3,7,3,8,3,2,9,6,2,8,3,9"
    let id : blade = parseSword sword1
    print $ valueOfSword $ buildSword blade

    print "P2"
    file2 <- readFile "quest05/part2.txt"
    let swordQualities2 = map (valueOfSword . buildSword . tail . parseSword) (lines file2)
    print $ maximum swordQualities2 - minimum swordQualities2

    print "P3"
    file3 <- readFile "quest05/part3.txt"
    let swords3 = zip [1..] $ map (buildSword . tail . parseSword) (lines file3)
    let sorted = sortBy (flip compareSwords) swords3
    print $ sum . zipWith (*) [1..] $ map fst sorted
