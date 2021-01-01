module BusSchedule where

import qualified Data.Text as Text
import Text.Read
import Data.Maybe

type BusTimes = (Integer, [Integer])
type BusSchedule = [BusTimes]

timesOnOrAfter :: BusSchedule -> Integer -> BusSchedule
timesOnOrAfter [] _ = []
timesOnOrAfter ((id, times) : busTimes) n =
  (id, filter (\val -> val >= n) times) : timesOnOrAfter busTimes n

closestNextBus :: BusSchedule -> (Integer, Integer)
closestNextBus [] = error "no buses"
closestNextBus bs@((id1, times1):_) = closestNextBusHelper bs (id1, head times1)

closestNextBusHelper :: BusSchedule -> (Integer, Integer) -> (Integer, Integer)
closestNextBusHelper [] best = best
closestNextBusHelper ((busId, bTimes):bt) (bestId, bestTime) =
  closestNextBusHelper bt acc
  where
    acc = if (head bTimes) < bestTime then (busId, (head bTimes)) else (bestId, bestTime)

-- generation

generateBusSchedule :: String -> BusSchedule
generateBusSchedule s = map generateBusTimes (readIds s)

readIds :: String -> [Integer]
readIds s = validIds
  where
    ids = Text.splitOn (Text.pack ",") (Text.pack s)
    validIds = mapMaybe (\s -> readMaybe (Text.unpack s) :: Maybe Integer) ids

generateBusTimes :: Integer -> BusTimes
generateBusTimes n = (n , [x * n | x <- [0..]])
