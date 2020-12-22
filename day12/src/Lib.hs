module Lib where

import Navigation
import Problem1
import Problem2

solve1 :: IO()
solve1 = do 
    instructions <- readInput
    let finalShipPos@(dir, (x,y)) = foldl moveShip shipStart instructions
    let manhattenDist = abs x + abs y
    print manhattenDist

solve2 :: IO()
solve2 = do 
    instructions <- readInput
    let finalShipPos@((_, (x, y)), _) = foldl moveShipByWapoint startShipWaypoint  instructions
    let manhattenDist = abs x + abs y
    print manhattenDist


readInput :: IO [Instruction] 
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseInput contents of
        Left e -> error $ show e
        Right instructions -> return instructions