module Problem2 where 

import qualified Data.Text as Text
import Text.Read
import Data.Maybe

type BusID = Integer 
type MinuteOffset = Integer 
type BusOffset = (BusID, MinuteOffset)

findEarliestTime :: [BusOffset] -> Integer 
findEarliestTime = findEarliestTimeHelper 0 

findEarliestTimeHelper :: Integer -> [BusOffset] -> Integer
findEarliestTimeHelper t busOffsets = 
  if allBusesSubsequentlyDepartAtTime t busOffsets 
    then t 
    else findEarliestTimeHelper (t + 1) busOffsets

allBusesSubsequentlyDepartAtTime :: Integer -> [BusOffset] -> Bool 
allBusesSubsequentlyDepartAtTime t = all (busDepartsAtTime t)

busDepartsAtTime :: Integer -> BusOffset -> Bool 
busDepartsAtTime t (busId, offset) = mod (t + offset) busId == 0

generateBusOffsets :: String -> [BusOffset]
generateBusOffsets s = keepConstrainedBuses allBusOffsets
    where
        busIds = readIds s
        allBusOffsets = zip busIds [0..]

keepConstrainedBuses :: [(Maybe BusID, MinuteOffset)] -> [BusOffset]
keepConstrainedBuses [] = []
keepConstrainedBuses ((Nothing, _) : xs) = keepConstrainedBuses xs 
keepConstrainedBuses ((Just busId, offset) : xs) = (busId, offset) : keepConstrainedBuses xs 

readIds :: String -> [Maybe BusID]
readIds s = map (\s -> readMaybe s :: Maybe Integer) buses
  where
    buses = splitCommaList s
    
splitCommaList :: String -> [String]
splitCommaList s = subStrings
  where
    subTexts = Text.splitOn (Text.pack ",") (Text.pack s)
    subStrings = map Text.unpack subTexts
    