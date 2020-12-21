module Lib where

import SeatingPlan
import Rules1

solve1 :: IO()
solve1 = do 
    sp <- readInput
    solve1Looper 0 sp

solve1Looper :: Int -> SeatingPlan -> IO()
solve1Looper iteration sp = do 
    print $ "iteration " ++ show iteration
    let nextSP = updateSeatingPlan sp 
    if sp == nextSP then print (getNumOccupiedSeats nextSP) else solve1Looper (iteration + 1) nextSP

readInput :: IO SeatingPlan 
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let rows = lines contents
    return $ createSeatingPlan rows