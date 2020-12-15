module Lib where

import Instructions

solve1 :: IO()
solve1 = do 
    bootCode <- readInput
    let accBeforeLoop = runUntilLoop bootCode
    print accBeforeLoop

readInput :: IO BootCode
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseBootCode contents of 
        Left e -> error $ show e 
        Right ins -> return ins