module Lib where

import GroupAnswers

solve1 :: IO()
solve1 = do
    input <- readInput
    let groups = allGroupsParser input
    let uniqueGroupsAnswers = map getAGroupsUniqueAnswers groups
    let uniqueGroupsAnswersCount = map length uniqueGroupsAnswers
    let sumOfGroupAnswers = sum uniqueGroupsAnswersCount
    print sumOfGroupAnswers


solve2 :: IO()
solve2 = do
    input <- readInput
    let groups = allGroupsParser input
    let answersEveryoneInTheGroupHad = map answersEveryoneHad groups
    let answersEveryoneInTheGroupHadCount = map length answersEveryoneInTheGroupHad
    let sumOfGroupAnswers = sum answersEveryoneInTheGroupHadCount
    print sumOfGroupAnswers

readInput :: IO String
readInput = do
    let file = "input.txt"
    contents <- readFile file
    return contents