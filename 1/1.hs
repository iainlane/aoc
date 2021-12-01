dayone n t ns = [product x | x <- sequence . take n $ repeat ns, sum x == t]

one = dayone 2
two = dayone 3

main :: IO ()
main = do
    ls <- readFile "input.txt"
    let ns = map read $ lines ls
    case (one 2020 ns) of
        [] -> putStrLn "no result found 1"
        (x:xs) -> putStrLn $ show x
    case (two 2020 ns) of
        [] -> putStrLn "no result found 2"
        (x:xs) -> putStrLn $ show x
