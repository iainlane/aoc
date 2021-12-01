import Data.List.Split (splitWhen)
import Data.Set (fromList, intersection, unions)

intersections = unions >>= foldl intersection

one = sum . map (length . fromList . concat)
two = sum . map (length . intersections . (map fromList))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = splitWhen null $ lines contents
    putStrLn . show $ one ls
    putStrLn . show $ two ls
