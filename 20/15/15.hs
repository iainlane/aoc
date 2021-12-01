import qualified Data.Vector.Unboxed.Mutable as MV (replicate, read, write)
import Data.Vector.Unboxed.Mutable (MVector)
import Control.Monad
import Control.Monad.ST

input :: [Int]
input = [6, 3, 15, 13, 1, 0]

initVector :: Int -> [(Int, Int)] -> ST s (MVector s Int)
initVector n vals = do
    result <- MV.replicate n (-1)
    forM_ vals $ \(k, v) -> MV.write result k v
    return result

run :: Int -> (MVector s Int) -> Int -> Int -> ST s Int
run n v last pos
    | pos == n = return last
    | otherwise = do
        prev <- MV.read v last
        let diff = if prev == (-1) then 0 else (pos - prev)
        MV.write v last pos
        run n v diff (pos + 1)

sol :: Int -> [Int] -> Int
sol n input = runST $ do
        v <- initVector n (zip input [1..])
        run n v 0 (length input + 1)

one = sol 2020
two = sol 30000000

main :: IO ()
main = do
        print $ one input
        print $ two input
