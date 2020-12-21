module SeatingPlan where 



type Coord = (Int, Int)
data SeatState = Empty | Occupied | Floor deriving (Eq, Show)
type SeatingPlan = [(Coord, SeatState)]

getNumOccupiedSeats :: SeatingPlan -> Int 
getNumOccupiedSeats sp = length $ filter (== Occupied) $ map snd sp

-- parsing

charToSeatState :: Char -> SeatState
charToSeatState '.' = Floor
charToSeatState 'L' = Empty
charToSeatState '#' = Occupied
charToSeatState x = error $  "unexpected char " ++ [x] ++ " when parsing SeatState"

createSeatingPlanHelper :: Coord -> [String] -> SeatingPlan
createSeatingPlanHelper _ [] = []
createSeatingPlanHelper (x, y) ([] : rs) = createSeatingPlanHelper (0, y+1) rs
createSeatingPlanHelper (x, y) ((s : ss) : rs) = ((x, y), charToSeatState s) : createSeatingPlanHelper (x+1, y) (ss : rs)

createSeatingPlan :: [String] -> SeatingPlan
createSeatingPlan [] = []
createSeatingPlan input = createSeatingPlanHelper (0, 0) input
