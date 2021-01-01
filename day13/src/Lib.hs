module Lib where

import BusSchedule

solve1 :: IO()
solve1 = do
  inputLines <- readInput
  let departTime = read (inputLines!!0) :: Integer
  let busSchedule = generateBusSchedule (inputLines!!1)
  let scheduleOnOrAfterDepart = timesOnOrAfter busSchedule departTime
  let (bestbus, bestTime) = closestNextBus scheduleOnOrAfterDepart
  print $ show (bestbus, bestTime)
  let answer = bestbus * (bestTime - departTime)
  print $ show answer

readInput :: IO [String]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let allLines = lines contents
    return allLines
