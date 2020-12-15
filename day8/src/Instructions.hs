module Instructions where

import Text.ParserCombinators.Parsec

data Operation = ACC | JMP | NOP deriving (Show, Eq)
type Instruction = (Operation, Int)
type BootCode = [Instruction]

instance Read Operation where 
    readsPrec _ ('a':'c':'c':rest) = [(ACC, rest)] 
    readsPrec _ ('j':'m':'p':rest) = [(JMP, rest)] 
    readsPrec _ ('n':'o':'p':rest) = [(NOP, rest)] 
    readsPrec _ _ = []

runUntilTerm :: BootCode -> (Bool, Int)
runUntilTerm bc = runAndAccUntilTerm bc [] 0 0

runAndAccUntilTerm :: BootCode -> [Int] -> Int -> Int -> (Bool, Int)
runAndAccUntilTerm bc visited index acc
    | index == length bc = (True, acc)
    | index `elem` visited = (False, acc)
    | otherwise = runAndAccUntilTerm bc (index:visited) nextIndex newAcc
    where 
        currentInstr = bc!!index 
        (nextIndex, newAcc) = nextIndexAcc currentInstr (index, acc)

nextIndexAcc :: Instruction -> (Int, Int) -> (Int, Int) 
nextIndexAcc (NOP, _) (index, acc) = (index+1, acc)
nextIndexAcc (ACC, val) (index, acc) = (index+1, acc + val)
nextIndexAcc (JMP, val) (index, acc) = (index+val, acc)

changeInstructionOpAtIndex :: BootCode -> Int -> (Bool, BootCode)
changeInstructionOpAtIndex bc index 
    | not (canChangeInstr currentInstr) = (False, bc)
    | otherwise = (True, changeElemAt index newInstr bc)
    where 
        currentInstr = bc!!index
        newInstr = changeInstructionOp currentInstr

canChangeInstr :: Instruction -> Bool
canChangeInstr (ACC, _ ) = False
canChangeInstr _ = True

changeInstructionOp :: Instruction -> Instruction
changeInstructionOp instr@(ACC, _) = instr
changeInstructionOp (JMP, val) = (NOP, val)
changeInstructionOp (NOP, val) = (JMP, val)

changeElemAt :: Int -> a -> [a] -> [a] 
changeElemAt 0 val (_ : xs) = val : xs
changeElemAt _ _ [] = error "index to change elem at is larger than the list length"
changeElemAt acc val (x : xs) = x : changeElemAt (acc - 1) val xs

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
