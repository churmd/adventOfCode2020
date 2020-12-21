module Rules1 where 

import SeatingPlan
import Data.Maybe as Maybe
import Data.List as List

getAllAdjacentCoords :: Coord -> [Coord]
getAllAdjacentCoords (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x+1,y-1), (x-1,y+1), (x-1,y-1)]

updateSeat :: SeatingPlan -> (Coord, SeatState) -> (Coord, SeatState)
updateSeat sp (coord, currentSeatState) = (coord, updatedSeatState)
    where
        updatedSeatState = updateSeatHelper sp currentSeatState adjacentSeatStates
        adjacentSeatStates = Maybe.mapMaybe (`lookup` sp) (getAllAdjacentCoords coord)

updateSeatHelper :: SeatingPlan -> SeatState -> [SeatState] -> SeatState
updateSeatHelper sp Empty adjacentSeats = 
    if Occupied `notElem` adjacentSeats then Occupied else Empty
updateSeatHelper sp Occupied adjacentSeats = 
    if  length (filter (== Occupied) adjacentSeats) >= 4 then Empty else Occupied
updateSeatHelper _ seat _ = seat

updateSeatingPlan :: SeatingPlan -> SeatingPlan
updateSeatingPlan sp = map (\(coord, ss) -> updateSeat sp (coord, ss)) sp

solveFinalSeatingPlanHelper :: (SeatingPlan, SeatingPlan) -> SeatingPlan
solveFinalSeatingPlanHelper (prev, current) = 
    if prev == current then current else solveFinalSeatingPlanHelper (current, updateSeatingPlan current)

solveFinalSeatingPlan :: SeatingPlan -> SeatingPlan
solveFinalSeatingPlan sp = solveFinalSeatingPlanHelper (sp, updateSeatingPlan sp)