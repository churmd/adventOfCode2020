module Rules2 where 

import SeatingPlan
import Data.Maybe as Maybe
import Data.List as List

type Direction = Coord -> Coord

allDirections :: [Direction]
allDirections = 
    [\(x,y) -> (x+1,y), 
    \(x,y) -> (x-1,y), 
    \(x,y) -> (x,y+1), 
    \(x,y) -> (x,y-1), 
    \(x,y) -> (x+1,y+1), 
    \(x,y) -> (x+1,y-1), 
    \(x,y) -> (x-1,y+1), 
    \(x,y) -> (x-1,y-1)]

findFirstSeatInDirection :: SeatingPlan  -> Coord -> Direction -> Maybe SeatState 
findFirstSeatInDirection sp coord dir = 
    case lookup (dir coord) sp of 
        Nothing -> Nothing 
        Just Floor -> findFirstSeatInDirection sp (dir coord) dir
        Just st -> Just st

getAllSurroundingSeats :: SeatingPlan -> Coord -> [SeatState]
getAllSurroundingSeats sp coord = Maybe.mapMaybe (findFirstSeatInDirection sp coord) allDirections


getAllAdjacentCoords :: Coord -> [Coord]
getAllAdjacentCoords (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x+1,y-1), (x-1,y+1), (x-1,y-1)]

updateSeat :: SeatingPlan -> (Coord, SeatState) -> (Coord, SeatState)
updateSeat sp (coord, currentSeatState) = (coord, updatedSeatState)
    where
        updatedSeatState = updateSeatHelper sp currentSeatState surroundingSeats
        surroundingSeats = getAllSurroundingSeats sp coord

updateSeatHelper :: SeatingPlan -> SeatState -> [SeatState] -> SeatState
updateSeatHelper sp Empty adjacentSeats = 
    if Occupied `notElem` adjacentSeats then Occupied else Empty
updateSeatHelper sp Occupied adjacentSeats = 
    if  length (filter (== Occupied) adjacentSeats) >= 5 then Empty else Occupied
updateSeatHelper _ seat _ = seat

updateSeatingPlan :: SeatingPlan -> SeatingPlan
updateSeatingPlan sp = map (\(coord, ss) -> updateSeat sp (coord, ss)) sp

solveFinalSeatingPlanHelper :: (SeatingPlan, SeatingPlan) -> SeatingPlan
solveFinalSeatingPlanHelper (prev, current) = 
    if prev == current then current else solveFinalSeatingPlanHelper (current, updateSeatingPlan current)

solveFinalSeatingPlan :: SeatingPlan -> SeatingPlan
solveFinalSeatingPlan sp = solveFinalSeatingPlanHelper (sp, updateSeatingPlan sp)