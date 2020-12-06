import Data.List.Split (splitWhen)
import Data.Set (Set, empty, fromList, intersection, isSubsetOf, unions)

intersections xs = foldl intersection (unions xs) xs

one = sum . map (length . fromList . concat)
two = sum . map (length . intersections . (map fromList))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = splitWhen null $ lines contents
    putStrLn . show $ one ls
    putStrLn . show $ two ls
