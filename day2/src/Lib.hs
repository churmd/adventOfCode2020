module Lib where

import PasswordRecord    

solve1 :: IO()
solve1 = do 
    passwordRecords <- readInput
    let validCount = countValidPasswords passwordRecords isValidProblem1
    print validCount

solve2 :: IO()
solve2 = do 
    passwordRecords <- readInput
    let validCount = countValidPasswords passwordRecords isValidProblem2
    print validCount

readInput :: IO[PasswordRecord]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseRules contents of
        Left e -> error $ show e
        Right records -> return records


countValidPasswords :: [PasswordRecord] -> (PasswordRecord -> Bool) -> Int
countValidPasswords passwordRecords isValid=  length (filter isValid passwordRecords)