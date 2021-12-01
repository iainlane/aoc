data Point = Tree | Open
    deriving Show

newtype Forest = Forest [[Point]]

newtype Position = Position (Int, Int)
    deriving Show

parseChar :: Char -> Point
parseChar '.' = Open
parseChar '#' = Tree

parse :: String -> Forest
parse s = Forest $ map (map parseChar . cycle) ls
        where ls = lines s

move :: Forest -> (Int, Int) -> Position -> Maybe Position
move (Forest ps) (x, y) (Position ((cx, cy))) | ny < length ps = Just . Position $ (nx, ny)
                                              | otherwise      = Nothing
        where nx = cx + x
              ny = cy + y

pointAt :: (Int, Int) -> Forest -> Point
pointAt (x, y) (Forest ps) = (ps !! y) !! x

moveToEnd :: Forest -> (Int, Int) -> [Point]
moveToEnd f inc = moveToEnd' (Position (0, 0))
        where moveToEnd' p = case move f inc p of
                Nothing -> []
                Just p'@(Position (nx, ny)) -> (pointAt (nx, ny) f):(moveToEnd' p')

isTree :: Point -> Bool
isTree Tree = True
isTree Open = False

daythree f ps = product $ map (\p -> length . filter isTree $ moveToEnd f p) ps

one f = daythree f [(3, 1)]
two f = daythree f [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main = do
    ls <- readFile "input.txt"
    let parsed = parse ls
    putStrLn . show $ one parsed
    putStrLn . show $ two parsed
