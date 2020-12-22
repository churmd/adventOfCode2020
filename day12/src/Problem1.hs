module Problem1 where 

import Navigation

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