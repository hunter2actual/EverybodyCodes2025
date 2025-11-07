import Data.List
import Data.Text (splitOn, pack, unpack)

totalRatio :: [Double] -> Double
totalRatio xs = product $ zipWith (/) xs (tail xs)

totalRatio' :: [[Double]] -> Double
totalRatio' xs = product $ zipWith gearRatioDivision xs (tail xs)

parseRatios :: String -> [Double]
parseRatios x = map (read.unpack) (splitOn (pack "|") (pack x))

gearRatioDivision :: [Double] -> [Double] -> Double
gearRatioDivision x y
    | length x == 1 = x!!0 / y!!0
    | otherwise     = x!!1 / y!!0

main :: IO ()
main = do
    -- let sample1 = [128, 64, 32, 16, 8]
    -- let sample2 = [102, 75, 50, 35, 13]
    -- let sample3 = ["5", "5|10", "10|20", "5"]
    -- let sample4 = ["5", "7|21", "18|36", "27|27", "10|50", "10|50", "11"]

    -- P1
    print "P1"
    let gears1 = [1000, 998, 976, 964, 951, 928, 912, 890, 874, 848, 844, 821, 793, 781, 752, 751, 739, 714, 690, 678, 664, 649, 640, 616, 604, 592, 591, 570, 556, 528, 523, 520, 514, 497, 470, 461, 442, 415, 390, 373, 361, 333, 310, 307, 278, 251, 235, 226, 204, 175]
    print $ floor $ 2025 * totalRatio gears1

    -- P2
    print "P2"
    let gears2 = [970, 967, 939, 927, 904, 879, 878, 861, 836, 832, 821, 793, 771, 748, 747, 733, 706, 698, 690, 672, 655, 653, 642, 632, 629, 619, 614, 592, 574, 568, 548, 547, 530, 503, 488, 471, 466, 450, 430, 405, 396, 382, 368, 352, 335, 313, 297, 274, 259, 223]
    print $ ceiling $ (1e13 :: Double) / totalRatio gears2

    -- P3
    print "P3"
    let ratios = ["580","574|574","563|1126","557|557","544|1088","525|525","506|2024","498|498","497|1988","483|483","475|1425","457|457","448|448","445|445","437|437","435|435","420|420","412|412","407|1221","392|392","384|1152","380|380","372|1116","365|365","361|361","360|360","343|1029","328|328","327|327","326|326","311|933","306|306","291|582","282|282","279|558","272|272","266|1064","262|262","253|759","240|240","225|900","219|219","210|840","198|198","187|748","172|172","155|310","145|145","134|134","112"]
    print $ floor $ 100 * totalRatio' (map parseRatios ratios)
