module Navigation where 

import Text.ParserCombinators.Parsec

type Coord = (Int, Int)
data Direction = South | North | East | West deriving (Eq, Show)
data Action = GoNorth | GoSouth | GoEast | GoWest | TurnLeft | TurnRight | GoForward deriving (Eq, Show)
type Instruction = (Action, Int)
type Ship = (Direction, Coord)

directionToAction :: Direction -> Action 
directionToAction North = GoNorth
directionToAction South = GoSouth
directionToAction East = GoEast
directionToAction West = GoWest

turnShip :: Ship -> Action -> Ship 
turnShip (North, coords) TurnLeft = (West, coords)
turnShip (East, coords) TurnLeft = (North, coords)
turnShip (South, coords) TurnLeft = (East, coords)
turnShip (West, coords) TurnLeft = (South, coords)
turnShip (North, coords) TurnRight = (East, coords)
turnShip (East, coords) TurnRight = (South, coords)
turnShip (South, coords) TurnRight = (West, coords)
turnShip (West, coords) TurnRight = (North, coords)
turnShip s _ = s

shipStart :: Ship 
shipStart = (East, (0,0))

moveShip :: Ship -> Instruction -> Ship 
moveShip (dir, (x,y)) (GoNorth, amount) = (dir, (x, y + amount))
moveShip (dir, (x,y)) (GoSouth, amount) = (dir, (x, y - amount))
moveShip (dir, (x,y)) (GoEast, amount) = (dir, (x + amount, y))
moveShip (dir, (x,y)) (GoWest, amount) = (dir, (x - amount, y))
moveShip s@(dir, (x,y)) (GoForward, amount) = moveShip s (directionToAction dir, amount)
moveShip s (TurnLeft, 0) = s
moveShip s (TurnLeft, amount) = moveShip (turnShip s TurnLeft) (TurnLeft, amount - 90)
moveShip s (TurnRight, 0) = s
moveShip s (TurnRight, amount) = moveShip (turnShip s TurnRight) (TurnRight, amount - 90)

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