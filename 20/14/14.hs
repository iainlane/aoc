import Control.Monad.State
import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Map (Map)

import Debug.Trace

import Parser (Exp(..), parseLex)

import qualified Data.Map as Map

newtype MemState = MemState (String, Map Int Int)
        deriving Show

unMemState (MemState ms) = ms

keepOnly :: Char -> Char -> Char -> Char
keepOnly k r c | k == c = c
               | otherwise = r

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

oneMask = toDec . map (keepOnly '1' '0')
zeroMask = toDec . map (keepOnly '0' '1')

getAddressesToWrite mask address = map ((.|.) ((address .&. (complement . toDec $ map (keepOnly '0' '1' . keepOnly 'X' '0') mask)) .|. (oneMask mask))) (map toDec $ expandX mask)

applyMask mask n = ((oneMask mask) .|. n) .&. (zeroMask mask)

runProg :: (Exp -> State MemState ()) -> [Exp] -> Int
runProg f es = evalState (runProg' f es) initialState
        where
                initialState = MemState (mempty, Map.empty)

runProg' :: (Exp -> State MemState ()) -> [Exp] -> State MemState Int
runProg' _ [] = get >>= return . sum . Map.elems . snd . unMemState
runProg' f ((Mask m):is) = do
        (MemState (_, mem)) <- get
        put (MemState (m, mem))
        runProg' f is
runProg' f (m@(Mem loc val):is) = do
        f m
        runProg' f is

runMem :: Exp -> State MemState ()
runMem (Mem loc val) = do
        (MemState (mask, mem)) <- get
        let cur = Map.findWithDefault 0 loc mem
        let newMem = Map.insert loc (applyMask mask val) mem
        put (MemState (mask, newMem))

runMem2 :: Exp -> State MemState ()
runMem2 (Mem loc val) = do
        (MemState (mask, mem)) <- get
        let cur = Map.findWithDefault 0 loc mem
        let addresses = getAddressesToWrite mask loc
        let replace = Map.fromList $ zip addresses (repeat val)
        let newMem = Map.union replace mem
        put (MemState (mask, newMem))

expandX :: String -> [String]
expandX [] = [[]]
expandX ('X':s) = prepend '0' s ++ prepend '1' s
expandX (c:s) = prepend c s

prepend c s = map ((:) c) (expandX s)

one = runProg runMem
two = runProg runMem2

main = do
        contents <- readFile "input.txt"
        let exps = parseLex contents
        print $ one exps
        print $ two exps
