module Lib where

import Bags
import Data.List

solve1 :: IO()
solve1 = do 
    rules <- readInput
    let bagsThatCanContainShinyGold = map fst $ filter (\(r, canHold) -> canHold) $ rulesThatCanContainBag rules "shiny gold"
    print $ length bagsThatCanContainShinyGold

solve2 :: IO()
solve2 = do 
    rules <- readInput
    let shinyGoldRule = find (\(BagRule name _) -> name == "shiny gold") rules
    case shinyGoldRule of 
        Nothing -> error "Could not find shiny gold rule"
        Just r -> print $ numOfSubBags r rules


readInput :: IO [BagRule]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseBagRules contents of
        Left e -> error $ show e
        Right rules -> return rules

