data Instruction = North Int
                 | South Int
                 | East Int
                 | West Int
                 | TurnLeft Int
                 | TurnRight Int
                 | Forward Int
        deriving Show

newtype FerryState = FerryState ((Int, Int), (Int, Int), Int)
        deriving Show

initialState = FerryState ((0, 0), (10, 1), 90)

move :: FerryState -> Instruction -> FerryState
move (FerryState ((x, y), w, d)) (North n)       = FerryState ((x, y + n), w, d)
move (FerryState ((x, y), w, d)) (South n)       = FerryState ((x, y - n), w, d)
move (FerryState ((x, y), w, d)) (East n)        = FerryState ((x + n, y), w, d)
move (FerryState ((x, y), w, d)) (West n)        = FerryState ((x - n, y), w, d)
move f@(FerryState ((x, y), w, d)) (TurnLeft n)    = move f (TurnRight (360 - n))
move (FerryState ((x, y), w, d)) (TurnRight n)   = FerryState ((x, y), w, (d + n) `mod` 360)
move f@(FerryState ((x, y), w, 0)) (Forward n)   = move f (North n)
move f@(FerryState ((x, y), w, 90)) (Forward n)  = move f (East n)
move f@(FerryState ((x, y), w, 180)) (Forward n) = move f (South n)
move f@(FerryState ((x, y), w, 270)) (Forward n) = move f (West n)

moveW :: FerryState -> Instruction -> FerryState
moveW (FerryState (xy, (wx, wy), d)) (North n)       = FerryState (xy, (wx, wy + n), d)
moveW (FerryState (xy, (wx, wy), d)) (South n)       = FerryState (xy, (wx, wy - n), d)
moveW (FerryState (xy, (wx, wy), d)) (East n)        = FerryState (xy, (wx + n, wy), d)
moveW (FerryState (xy, (wx, wy), d)) (West n)        = FerryState (xy, (wx - n, wy), d)
moveW f@(FerryState ((x, y), (wx, wy), d)) (TurnLeft n)    = moveW f (TurnRight (360 - n))
moveW f@(FerryState ((x, y), (wx, wy), d)) (TurnRight 0)   = f
moveW (FerryState ((x, y), (wx, wy), d)) (TurnRight 90)   = FerryState ((x, y), (wy, wx * (-1)), d)
moveW f@(FerryState ((x, y), (wx, wy), d)) (TurnRight n)   = moveW (moveW f (TurnRight 90)) (TurnRight (n - 90))
moveW f@(FerryState ((x, y), (wx, wy), d)) (Forward n)   = FerryState ((x + (wx * n), y + (wy * n)), (wx, wy), d)

manhattanDistance :: FerryState -> Int
manhattanDistance (FerryState ((x, y), _, _)) = abs x + abs y

parseChar :: Char -> Int -> Instruction
parseChar 'N' = North
parseChar 'S' = South
parseChar 'E' = East
parseChar 'W' = West
parseChar 'L' = TurnLeft
parseChar 'R' = TurnRight
parseChar 'F' = Forward

parseLine :: String -> Instruction
parseLine (c:cs) = (parseChar c) (read cs)

parse :: String -> [Instruction]
parse = map parseLine . lines

sol :: (FerryState -> Instruction -> FerryState) -> String -> Int
sol f = manhattanDistance . foldl f initialState . parse

one = sol move
two = sol moveW

main :: IO ()
main = do
        contents <- readFile "input.txt"
        putStrLn . show $ one contents
        putStrLn . show $ two contents
