import Control.Monad.State
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

type Accumulator = Int
type ProgramCounter = Int

data Instruction = ACC Int
        | JMP Int
        | NOP Int
        deriving Show

type Program = [Instruction]

type ProgramState = (Accumulator, ProgramCounter, Set.Set ProgramCounter)

exec :: Instruction -> ProgramState -> ProgramState
exec (ACC n) (acc, pc, s) = (acc + n, pc + 1, Set.insert pc s)
exec (JMP n) (acc, pc, s) = (acc, pc + n, Set.insert pc s)
exec (NOP _) (acc, pc, s) = (acc, pc + 1, Set.insert pc s)

execProgram :: Program -> (Accumulator, Bool)
execProgram p = evalState (execProgramS p) (0, 0, Set.empty)

execProgramS :: Program -> State ProgramState (Accumulator, Bool)
execProgramS [] = do
        (acc, _, _) <- get
        return (acc, True)
execProgramS prog@(x:xs) = execProgramSAcc x
        where
        execProgramSAcc :: Instruction -> State ProgramState (Accumulator, Bool)
        execProgramSAcc p = do
                s@(acc, pc, visited) <- get
                let s'@(acc', pc', visited') = exec p s
                case Set.isProperSubsetOf visited visited' of
                        True -> do
                                put s'
                                case pc' == (length prog) of
                                        True -> return (acc', True)
                                        False -> execProgramSAcc (prog !! pc')
                        False -> return (acc, False)

--generateMutations :: Program -> [Program]
generateMutations p = generateMutationsAcc p [[]]
        where
                generateMutationsAcc [] sofar = sofar
                generateMutationsAcc ((ACC n):prog) sofar = generateMutationsAcc prog (map (flip (++) [(ACC n)]) sofar)
                generateMutationsAcc (n:prog) sofar = (map (flip (++) ((opp n):prog)) sofar) ++
                                                      (generateMutationsAcc prog (map (flip (++) [n]) sofar))
                opp (NOP n) = JMP n
                opp (JMP n) = NOP n

parse:: [String] -> Program
parse [] = []
parse (x:xs) = parseCommand c v:parse xs
        where (c:v:_) = words x
              stripPlus orig = fromMaybe orig $ stripPrefix "+" orig
              parseCommand "nop" n = NOP (read $ stripPlus n)
              parseCommand "acc" n = ACC (read $ stripPlus n)
              parseCommand "jmp" n = JMP (read $ stripPlus n)

one = fst . execProgram . parse
two = fst . head . filter ((== True) . snd) . map execProgram . generateMutations . parse

main :: IO ()
main = do
        contents <- readFile "input.txt"
        let ls = lines contents
        putStrLn . show $ one ls
        putStrLn . show $ two ls
