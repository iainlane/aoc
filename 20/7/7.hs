import Control.Arrow
import Data.List (foldl')
import Data.List.Split
import Data.Map (Map, empty, foldrWithKey, findWithDefault, fromList, insert, (!))
import qualified Data.Set as Set

type Colour = String

-- common

makeBagMap :: [(Colour, [(Int, Colour)])] -> Map Colour [(Int, Colour)]
makeBagMap = fromList

invertBagMap :: Map Colour [(Int, Colour)] -> Map Colour (Set.Set Colour)
invertBagMap = foldrWithKey invert empty
    where
        invert :: Colour -> [(Int, Colour)] -> Map Colour (Set.Set Colour) -> Map Colour (Set.Set Colour)
        invert c [] input = input
        invert c ((_, v):vs) input = invert c vs (insert v new input)
                where current = findWithDefault Set.empty v input
                      new = Set.insert c current

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

-- eek, this is janky, sorry
parse :: [String] -> [(Colour, [(Int, Colour)])]
parse [] = []
parse (x:xs) = (k, val):parse xs
    where (v:vs@(b:bs)) = splitOnAnyOf ["contain", ","] x
          wv = words v
          k = wv !! 0 ++ " " ++ wv !! 1
          val = case b of
            " no other bags." -> []
            _ -> map ((((read . head) &&& (\w -> (w !! 1 ++ " " ++ w !! 2))) . words)) vs

-- one

findBagsWhichCanContain :: Colour -> Map Colour (Set.Set Colour) -> Set.Set Colour
findBagsWhichCanContain c m = fBAcc m c Set.empty
    where
        fBAcc :: Map Colour (Set.Set Colour) -> Colour -> Set.Set Colour -> Set.Set Colour
        fBAcc m c s = Set.union newColours . Set.unions $ (Set.map (\v -> fBAcc m v allColours)) newColours
            where newColours = Set.difference (findWithDefault Set.empty c m) s
                  allColours = Set.union s newColours

-- two

getNumContainedBags :: Colour -> Map Colour [(Int, Colour)] -> Int
getNumContainedBags c m = sum (map (\(a, b) -> a + a * (getNumContainedBags b m)) cb)
    where cb = findWithDefault [] c m

one colour = Set.size . findBagsWhichCanContain colour . invertBagMap . makeBagMap . parse
two colour = getNumContainedBags colour . makeBagMap . parse

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    putStrLn . show $ one "shiny gold" ls
    putStrLn . show $ two "shiny gold" ls
