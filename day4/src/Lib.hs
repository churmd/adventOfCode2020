module Lib where

import Passport

solve1 :: IO ()
solve1 = do
    passports <- readInput
    let validPassports = filter hasRequiredFields passports
    print $ length validPassports

solve2 :: IO ()
solve2 = do
    passports <- readInput
    let validPassports = filter passesAllRules passports
    print $ length validPassports

readInput :: IO [Passport]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let passports = parsePassports contents
    case passports of
        Left e -> error $ show e
        Right ps -> return ps