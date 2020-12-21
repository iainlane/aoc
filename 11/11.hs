import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&))
import Data.List (intersperse, maximumBy)
import Data.Map (Map, (!))
import Data.Ord (comparing)

import Debug.Trace

data Cell = Occupied | Empty | Floor
        deriving Eq

instance (Show Cell) where
        show Occupied = "#"
        show Empty = "L"
        show Floor = "."

newtype Grid = Grid (Map (Int, Int) Cell)
        deriving (Eq)

instance Show Grid where
        show = showGrid

showGrid g = concat . intersperse "\n" . reverse $ showGrid' g 0 []
        where
                showGrid' g n xs | null (line n) = xs
                                 | otherwise = showGrid' g (n+1) ((line n):xs)
                line n = showLine g n

showLine :: Grid -> Int -> String
showLine (Grid g) n = concatMap show . Map.elems $ Map.filterWithKey (\(x, y) v -> y == n) g

step :: (Grid -> (Int, Int) -> Cell -> Cell) -> Grid -> Grid
step stepFn gr@(Grid g) = Grid $ Map.mapWithKey (stepFn gr) g

stepOne :: (Grid -> (Int, Int) -> Int) -> Int -> Grid -> (Int, Int) -> Cell -> Cell
stepOne f _ g xy Empty    | f g xy == 0 = Occupied
stepOne f t g xy Occupied | f g xy >= t = Empty
stepOne _ _ g xy c        | otherwise = c

occupiedNeighbours :: (Grid -> (Int, Int) -> [Cell]) -> Grid -> (Int, Int) -> Int
occupiedNeighbours f g@(Grid grid) = length .
                                   filter (== Occupied) .
                                   (f g)

neighbourCells :: Grid -> (Int, Int) -> [Cell]
neighbourCells (Grid grid) (x, y) = map (\xy -> Map.findWithDefault Floor xy grid)
        [(x', y') | x' <- [x - 1, x, x + 1],
                    y' <- [y - 1, y, y + 1],
                    x' >= 0,
                    y' >= 0,
                    not (x == x' && y == y')
        ]

visibleCells :: Grid -> (Int, Int) -> [Cell]
visibleCells g (x, y) = map (findSeat g) ds
        where
                -- I would like to make the cartesian product of (+), (-),
                -- const and generate these lists that way, but this breaks for
                -- (const, const) and I don't know how to solve that nicely
                -- right now. Maybe if I exclude (x, y)?
                ds = [[(x - n, y - n) | n <- [1..]],
                      [(x, y - n)     | n <- [1..]],
                      [(x + n, y - n) | n <- [1..]],
                      [(x + n, y)     | n <- [1..]],
                      [(x + n, y + n) | n <- [1..]],
                      [(x, y + n)     | n <- [1..]],
                      [(x - n, y + n) | n <- [1..]],
                      [(x - n, y)     | n <- [1..]]]

findSeat :: Grid -> [(Int, Int)] -> Cell
findSeat g@(Grid grid) (xy:coords) = case Map.lookup xy grid of
        Nothing -> Floor
        (Just Floor) -> findSeat g coords
        (Just c) -> c

parse :: String -> Grid
parse s = Grid $ Map.fromList [((x, y), c) | (y, xc) <- zip [0..] . map (zip [0..]) $ map parseLine ls, (x, c) <- xc]
        where ls = lines s

parseLine :: String -> [Cell]
parseLine [] = []
parseLine ('L':xs) = Empty:(parseLine xs)
parseLine ('.':xs) = Floor:(parseLine xs)
parseLine ('#':xs) = Occupied:(parseLine xs)

nOccupied :: Grid -> Int
nOccupied (Grid g) = length . filter (== Occupied) . map snd $ Map.toList g

one :: Grid -> Int
one = sol (stepOne (occupiedNeighbours neighbourCells) 4)

two :: Grid -> Int
two = sol (stepOne (occupiedNeighbours visibleCells) 5)

sol :: (Grid -> (Int, Int) -> Cell -> Cell) -> Grid -> Int
sol fn g = nOccupied $ sol' (iterate (step fn) g)
        where
                sol' (x:y:xs) | x == y = y
                              | otherwise = sol' xs

main :: IO ()
main = do
        ls <- readFile "input.txt"
        putStrLn . show . one $ parse ls
        putStrLn . show . two $ parse ls
