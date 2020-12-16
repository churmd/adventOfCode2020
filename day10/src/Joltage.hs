module Joltage where

import qualified Data.Map as Map
import Data.Maybe

type Jolt = Int 
type JoltDifferences = Map.Map Jolt Int

joltDifferencesInList :: [Jolt] -> JoltDifferences
joltDifferencesInList joltList = joltDifferencesInListHelper joltList Map.empty 

joltDifferencesInListHelper :: [Jolt] -> JoltDifferences -> JoltDifferences
joltDifferencesInListHelper [] joltDifs = joltDifs
joltDifferencesInListHelper [j] joltDifs = joltDifs 
joltDifferencesInListHelper (j1 : j2 : js) joltDifs = joltDifferencesInListHelper (j2:js) (Map.insertWith (+) (j2 - j1) 1 joltDifs)

type PathsToJolt = Map.Map Jolt Int 

initPath :: PathsToJolt
initPath = Map.fromList [(0, 1)]

addJoltsToPath :: [Jolt] -> PathsToJolt -> PathsToJolt
addJoltsToPath [] currentPaths = currentPaths
addJoltsToPath (j : js) currentPaths 
    | Map.notMember j currentPaths = addJoltsToPath js currentPaths
    | otherwise = addJoltsToPath js updatedPaths
    where 
        currentJoltValue = fromMaybe 0 (Map.lookup j currentPaths)
        updatedPaths = Map.insertWith (+) (j+3) currentJoltValue (Map.insertWith (+) (j+2) currentJoltValue (Map.insertWith (+) (j+1) currentJoltValue currentPaths))