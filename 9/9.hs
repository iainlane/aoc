import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (inits, tails)

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

parts :: (Ord b, Num b) => Int -> [b] -> [([b], b)]
parts n = map ((\(a, b) -> (cprod a, head b)) . splitAt n) . windows (n + 1)
        where cprod l = [n + m | n <- l, m <- l, n /= m && n < m]

noSum :: Eq a => [([a], a)] -> [([a], a)]
noSum = filter (\(a, b) -> not $ any ((==) b) a)

sublists :: [a] -> [[a]]
sublists = concatMap tails . inits

one = snd . head . noSum . parts 25
two ls = head . (\oneAns -> [minimum xs + maximum xs | xs <- sublists ls, sum xs == oneAns, xs /= [oneAns]]) $ one ls

main :: IO ()
main = do
        contents <- readFile "input.txt"
        let ls = map (read :: String -> Int) $ lines contents
        putStrLn . show $ one ls
        putStrLn . show $ two ls
