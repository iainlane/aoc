import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Data.Map (Map, (!))
import Data.Set (Set)
import Debug.Trace

findBestAdapter :: Set Int -> Int -> Int
findBestAdapter s n | (n + 1) `Set.member` s = n + 1
                    | (n + 2) `Set.member` s = n + 2
                    | (n + 3) `Set.member` s = n + 3
                    | otherwise = error "no matching adapter"

canBeConnectedTo :: Set Int -> Int -> Set Int
canBeConnectedTo s n = Set.intersection s (Set.fromList [n - 1, n - 2, n - 3])

connectedMap :: Set Int -> Map Int (Set Int)
connectedMap as = Set.foldr (\elem map -> Map.insert elem (canBeConnectedTo as elem) map) Map.empty as

connectedMapWithWeights :: Set Int -> Map Int ((Int, Set Int))
connectedMapWithWeights = Map.map (max 1 . length &&& id) . connectedMap

nPaths :: Map Int (Set Int) -> Map Int (Int, Set Int)
nPaths m = Map.foldlWithKey (\accumMap key inputs -> (Map.insert key (f inputs accumMap, inputs) accumMap)) Map.empty m
        where
                f :: Set Int -> Map Int (Int, Set Int) -> Int
                f inputs accumMap = max 1 . sum $ map (fst . (!) accumMap) (Set.toList inputs)

findAdapterChain :: Set Int -> [Int]
findAdapterChain s = 0 : findAdapterChain' s 0
        where
                findAdapterChain' as start | next == max = [max, max + 3]
                                           | otherwise = next : findAdapterChain' as next
                        where
                                max = fromJust $ Set.lookupMax as
                                next = findBestAdapter as start

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (drop 1 xs) xs

counts :: (Ord a, Eq a) => [a] -> Map a Int
counts = Map.fromList . counts'
        where
                counts' [] = []
                counts' l@(x:xs) = (x, length (filter (== x) l)) : (counts' (filter (/= x) xs))

one = (\m -> m ! 1 * m ! 3) . counts . diffs . findAdapterChain
two ns = fst $ (nPaths $ connectedMap ns) ! (Set.findMax ns)

adapters :: FilePath -> IO (Set Int)
adapters f = do
        contents <- readFile f
        let ls = lines contents
        return . Set.insert 0 . Set.fromList $ map read ls

main :: IO ()
main = do
        as <- adapters "input.txt"
        putStrLn . show $ one as
        putStrLn . show $ two as
