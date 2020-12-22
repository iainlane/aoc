import Control.Arrow ((&&&))
import Data.List
import Data.List.Split (splitOn)
import Data.Ord

minsToWait :: Int -> Int -> Int
minsToWait now freq = (-now) `mod` freq

one time = uncurry (*) . minimumBy (comparing fst) . map (minsToWait time &&& id)

parse :: String -> [(Int, Int)]
parse = map (fst &&& (read . snd)) . filter (not . (== "x") . snd) . zip [0..] . splitOn ","

euclid :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
euclid rst@(r, s, t) (0, _, _) = rst
euclid (r, s, t) rst'@(r', s', t') = euclid rst' (r `mod` r', s - q * s', t - q * t')
        where
                q = r `div` r'

bezout :: (Int, Int) -> (Int, Int)
bezout (a, b) = (s, t)
        where
                (_, s, t) = euclid (a, 1, 0) (b, 0, 1)

pR :: [(Int, Int)] -> [(Int,Int)]
pR = map (\p@(i,b) -> (b, uncurry minsToWait p))

chineseRemainder :: ([Int], [Int]) -> Int
chineseRemainder (ps, rs) = x `mod` n
        where
                n = product ps
                ns = map (div n) ps
                ms = map (fst . bezout) (zip ns ps)
                x = sum $ zipWith3 (\a b c -> a * b * c) rs ms ns

two = chineseRemainder . unzip . pR

main :: IO ()
main = do
        contents <- readFile "input.txt"
        let (t:b:_) = lines contents
        let time = read t
        let buses = parse b
        print $ one time (map snd buses)
        print $ two buses
