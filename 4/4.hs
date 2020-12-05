import Data.Char
import Data.List
import Data.List.Split
import Data.Map (Map, foldrWithKey, fromList, member)

data PassportField =
    BirthYear |
    IssueYear |
    ExpirationYear |
    Height |
    HairColor |
    EyeColor |
    PassportID |
    CountryID
    deriving (Eq, Ord, Show)

newtype Passport = Passport (Map PassportField String)
    deriving Show

parseField :: String -> (PassportField, String)
parseField s = (pf field, tail value)
    where (field, value) = splitAt 3 s
          pf "byr" = BirthYear
          pf "iyr" = IssueYear
          pf "eyr" = ExpirationYear
          pf "hgt" = Height
          pf "hcl" = HairColor
          pf "ecl" = EyeColor
          pf "pid" = PassportID
          pf "cid" = CountryID
          pf s = error ("unknown " ++ s)

isValid1 :: Passport -> Bool
isValid1 (Passport m) = and $ map (flip member m) [BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportID]

isValidField :: PassportField -> String -> Bool
isValidField BirthYear s = rs >= 1920 && rs <= 2002
    where rs = (read s)
isValidField IssueYear s = rs >= 2010 && rs <= 2020
    where rs = (read s)
isValidField ExpirationYear s = rs >= 2020 && rs <= 2030
    where rs = (read s)
isValidField Height s = case (span isDigit s) of
                            (n, "cm") -> (read n) >= 150 && (read n) <= 193
                            (n, "in") -> (read n) >= 59 && (read n) <= 76
                            _ -> False
isValidField HairColor (s:t) = s == '#' && length t == 6 && all (\c -> isDigit c || c `elem` "abcdef") t
isValidField EyeColor s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField PassportID s = length s == 9 && all isDigit s
isValidField CountryID s = True

isValid2 :: Passport -> Bool
isValid2 p@(Passport m) = isValid1 p && foldrWithKey (\key value accum -> accum && isValidField key value) True m

parse :: String -> [Passport]
parse = map (Passport . fromList . map parseField) . rawpassports
    where rawpassports = map (concat . map words . intersperse " ") . splitWhen null . lines

one = length . filter isValid1 . parse
two = length . filter isValid2 . parse

main :: IO ()
main = do
    ls <- readFile "input.txt"
    print $ one ls
    print $ two ls
