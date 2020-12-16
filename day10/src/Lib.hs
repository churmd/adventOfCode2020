module Lib where

import Joltage
import Data.List
import Data.Maybe
import Data.Tree
import qualified Data.Map as Map

solve1:: IO()
solve1 = do 
    joltNums <- readInput
    let joltDifMap = joltDifferencesInList joltNums
    let difOf1JoltOccurances = fromMaybe 0 (Map.lookup 1 joltDifMap)
    let difOf3JoltOccurances = fromMaybe 0 (Map.lookup 3 joltDifMap)
    print $ difOf1JoltOccurances * difOf3JoltOccurances

solve2:: IO()
solve2 = do 
    joltNums <- readInput
    let pathsToJolt = addJoltsToPath joltNums initPath 
    let maxPath = maximum  $ map snd $ filter (\(j, _) -> j `elem` joltNums) (Map.toList pathsToJolt)
    print maxPath

readInput :: IO [Jolt] 
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let allLines = lines contents
    let joltNums = map (\line -> read line :: Int) allLines
    let wallJolt = 0
    let deviceJolt = maximum joltNums + 3
    let wallToDeviceJolts = wallJolt : deviceJolt : joltNums
    return $ sort wallToDeviceJolts
