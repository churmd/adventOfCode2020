module Instructions where

import Text.ParserCombinators.Parsec
import Data.List

data Operation = ACC | JMP | NOP deriving (Show, Eq)
type Instruction = (Operation, Int)
type BootCode = [Instruction]

instance Read Operation where 
    readsPrec _ ('a':'c':'c':rest) = [(ACC, rest)] 
    readsPrec _ ('j':'m':'p':rest) = [(JMP, rest)] 
    readsPrec _ ('n':'o':'p':rest) = [(NOP, rest)] 
    readsPrec _ _ = []


runUntilLoop :: BootCode -> Int
runUntilLoop bc = runAndAccUntilLoop bc [] 0 0

runAndAccUntilLoop :: BootCode -> [Int] -> Int -> Int -> Int
runAndAccUntilLoop bc visited index acc
    | index `elem` visited = acc
    | otherwise = runAndAccUntilLoop bc (index:visited) nextIndex newAcc
    where 
        currentInstr = bc!!index 
        (nextIndex, newAcc) = nextIndexAcc currentInstr (index, acc)

nextIndexAcc :: Instruction -> (Int, Int) -> (Int, Int) 
nextIndexAcc (NOP, _) (index, acc) = (index+1, acc)
nextIndexAcc (ACC, val) (index, acc) = (index+1, acc + val)
nextIndexAcc (JMP, val) (index, acc) = (index+val, acc)

-- parsing 

parseOp :: GenParser Char st Operation
parseOp = do 
    chars <- count 3 anyChar
    return (read chars :: Operation)

parseInstr :: GenParser Char st Instruction
parseInstr = do 
    op <- parseOp
    space
    optional (char '+')
    num <- many (noneOf "\n")
    let val = read num :: Int
    return (op, val)

parseAllInstr :: GenParser Char st BootCode
parseAllInstr = endBy parseInstr newline

parseBootCode :: [Char] -> Either ParseError BootCode
parseBootCode input = parse  parseAllInstr "Error parsing" input
