import Data.Set (difference, fromList, toList)

data BTree a = Leaf a | Node (BTree a) (BTree a)
    deriving Show

mkTree n = mktree' [0..n]
        where mktree' [] = error "can't make empty tree"
              mktree' [x] = Leaf x
              mktree' xs@(x:y:_) = Node (mktree' lhs) (mktree' rhs)
                where (lhs, rhs) = splitAt ((length xs + 1) `div` 2) xs

left :: BTree a -> BTree a
left (Node l _) = l

right :: BTree a -> BTree a
right (Node _ r) = r

step :: Char -> BTree a -> BTree a
step 'F' = left
step 'L' = left
step 'B' = right
step 'R' = right

position :: (Enum a, Num a) => String -> a
position s = case position' s tree of
    Leaf n -> n
    _ -> error "argh"
    where
        tree = mkTree (2 ^ (length s) - 1)
        position' [] t = t
        position' (m:ms) t = position' ms (step m t)

seatId s = position row * 8 + position column
    where (row, column) = splitAt 7 s

one :: (Ord a, Num a, Enum a) => [String] -> a
one = maximum . map seatId

two ls = head . toList $ difference (fromList [min..max]) (fromList seatIds)
    where
        seatIds = map seatId ls
        min = minimum seatIds
        max = maximum seatIds

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    putStrLn . show . one $ ls
    putStrLn . show . two $ ls
