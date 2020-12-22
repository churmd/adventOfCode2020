module Problem2 where 

import Navigation

type Waypoint = Coord
type ShipWaypoint = (Ship, Waypoint)

startShipWaypoint :: ShipWaypoint
startShipWaypoint = ((East , (0,0)), (10, 1))

rotateWaypoint:: Waypoint -> Float -> Waypoint
rotateWaypoint (x, y) angle = (round newX , round newY)
    where 
        floatX = fromIntegral x :: Float
        floatY = fromIntegral y :: Float
        angleRadians = angle * (pi/180)
        newX = (floatX * cos angleRadians) - (floatY * sin angleRadians)
        newY = (floatX * sin angleRadians) + (floatY * cos angleRadians)

moveShipByWapoint :: ShipWaypoint -> Instruction -> ShipWaypoint
moveShipByWapoint (ship, (wx,wy)) (GoNorth, amount) = (ship, (wx, wy + amount))
moveShipByWapoint (ship, (wx,wy)) (GoSouth, amount) = (ship, (wx, wy - amount))
moveShipByWapoint (ship, (wx,wy)) (GoEast, amount) = (ship, (wx + amount, wy))
moveShipByWapoint (ship, (wx,wy)) (GoWest, amount) = (ship, (wx - amount, wy))
moveShipByWapoint (ship, wp) (TurnRight , amount) = (ship, rotateWaypoint wp (fromIntegral (-amount) :: Float))
moveShipByWapoint (ship, wp) (TurnLeft  , amount) = (ship, rotateWaypoint wp (fromIntegral amount :: Float))
moveShipByWapoint ((dir, (sx, sy)), (wx,wy)) (GoForward , amount) = ((dir, (sx + (amount * wx), sy + (amount * wy))), (wx, wy))
