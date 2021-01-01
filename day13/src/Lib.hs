module Lib where

readInput :: IO [String]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let allLines = lines contents
    return allLines
