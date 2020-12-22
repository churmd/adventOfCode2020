module Navigation where 

import Text.ParserCombinators.Parsec

type Coord = (Int, Int)
data Direction = South | North | East | West deriving (Eq, Show)
data Action = GoNorth | GoSouth | GoEast | GoWest | TurnLeft | TurnRight | GoForward deriving (Eq, Show)
type Instruction = (Action, Int)
type Ship = (Direction, Coord)

shipStart :: Ship 
shipStart = (East, (0,0))

-- parsing

charToAction :: Char -> Action 
charToAction 'N' = GoNorth
charToAction 'S' = GoSouth
charToAction 'E' = GoEast
charToAction 'W' = GoWest
charToAction 'L' = TurnLeft
charToAction 'R' = TurnRight
charToAction 'F' = GoForward
charToAction c = error $ "unexpected Action character " ++ [c]

number :: GenParser Char st Int
number = do 
    number <- many1 digit
    return (read number :: Int)

parseInstruction :: GenParser Char st Instruction
parseInstruction = do 
    actionChar <- anyChar 
    let action = charToAction actionChar 
    amount <- number
    return (action, amount)

parseAllInstructions :: GenParser Char st [Instruction]
parseAllInstructions = sepBy parseInstruction newline 

parseInput :: [Char] -> Either ParseError [Instruction]
parseInput input = parse parseAllInstructions "Error parsing" input