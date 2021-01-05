module Lib where

import BusSchedule
import Problem2

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

solve2 :: IO()
solve2 = do
  inputLines <- readInput
  let busOffsets = generateBusOffsets (inputLines!!1)
  let timestamp = findEarliestTime busOffsets
  print $ show timestamp

readInput :: IO [String]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let allLines = lines contents
    return allLines
