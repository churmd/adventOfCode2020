module Lib where

import SeatingPlan
import Rules1 as R1
import Rules2 as R2

solve1 :: IO()
solve1 = do 
    sp <- readInput
    let finalSeatingPlan = R1.solveFinalSeatingPlan  sp
    print (getNumOccupiedSeats finalSeatingPlan)

solve1Looper :: Int -> SeatingPlan -> IO()
solve1Looper iteration sp = do 
    print $ "iteration " ++ show iteration
    let nextSP = R1.updateSeatingPlan sp 
    if sp == nextSP then print (getNumOccupiedSeats nextSP) else solve1Looper (iteration + 1) nextSP

solve2 :: IO()
solve2 = do 
    sp <- readInput
    -- solve2Looper 0 sp
    let finalSeatingPlan = R2.solveFinalSeatingPlan  sp
    print (getNumOccupiedSeats finalSeatingPlan)

solve2Looper :: Int -> SeatingPlan -> IO()
solve2Looper iteration sp = do 
    print $ "iteration " ++ show iteration
    let nextSP = R2.updateSeatingPlan sp 
    if sp == nextSP then print (getNumOccupiedSeats nextSP) else solve2Looper (iteration + 1) nextSP

readInput :: IO SeatingPlan 
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let rows = lines contents
    return $ createSeatingPlan rows