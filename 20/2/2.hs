import Control.Arrow
import Data.List hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Text.Regex

data PasswordConstraint = PasswordConstraint Int Int Char String
    deriving Show

frequency :: Ord a => [a] -> Map a Int
frequency = fromList . map (head &&& length) . group . sort

parse :: String -> PasswordConstraint
parse s = PasswordConstraint (read l) (read h) c password
    where
        (l:h:(c:_):_:password:_) = splitRegex (mkRegex "[- :]") s

isValid1 :: PasswordConstraint -> Bool
isValid1 (PasswordConstraint lower upper char password) = maybe False (\n -> lower <= n && n <= upper) noccurrences
    where fm = frequency password
          noccurrences = lookup char fm

isValid2 :: PasswordConstraint -> Bool
isValid2 (PasswordConstraint start end char password) = (startgood || endgood) && not (startgood && endgood)
    where startgood = password !! (start - 1) == char
          endgood = password !! (end - 1) == char

solutions f = length . filter (== True) . map (f . parse) . lines

one = solutions isValid1
two = solutions isValid2

main = do
    ls <- readFile "input.txt"
    putStrLn . show $ one ls
    putStrLn . show $ two ls
