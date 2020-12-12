module Lib where

import Bags

solve1 :: IO()
solve1 = do 
    rules <- readInput
    let bagsThatCanContainShinyGold = map fst $ filter (\(r, canHold) -> canHold) $ rulesThatCanContainBag rules "shiny gold"
    print $ length bagsThatCanContainShinyGold

readInput :: IO [BagRule]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseBagRules contents of
        Left e -> error $ show e
        Right rules -> return rules

