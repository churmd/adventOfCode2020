module Lib where

import Instructions

solve1 :: IO()
solve1 = do 
    bootCode <- readInput
    let (_, accFinalVal) = runUntilTerm bootCode
    print accFinalVal

solve2 :: IO()
solve2 = do 
    bootCode <- readInput
    let alteredBootCodes = map snd $ filter fst $ map (changeInstructionOpAtIndex bootCode) [0..(length bootCode - 1)]
    let terminatingBootCodes = map snd $ filter fst $ map runUntilTerm alteredBootCodes
    print $ head terminatingBootCodes

readInput :: IO BootCode
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseBootCode contents of 
        Left e -> error $ show e 
        Right ins -> return ins