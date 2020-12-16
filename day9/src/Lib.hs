module Lib where

import XMAS
import Data.Bifunctor

solve1 :: IO Int
solve1 = do 
    xmasData <- readInput
    let all25NumsBeforeIndex = map (get25NumsBeforeIndex xmasData) [25..(xmasDataLen xmasData - 1)]
    let allPairingsBeforeIndex = map (Data.Bifunctor.first getAllPairings) all25NumsBeforeIndex
    let anySumEqualToIndex = map (\(leading25NumsPairs, indexVal) -> (anyPairingsSumTo leading25NumsPairs indexVal, indexVal)) allPairingsBeforeIndex
    let noSumEqualIndex = filter (not . fst) anySumEqualToIndex
    return $ snd $ head noSumEqualIndex

solve2 :: Int -> IO()
solve2 p1Solution = do
    (preamble, numSeq) <- readInput
    let fullSeq = preamble ++ numSeq
    let possibleSubLists = allSubLists fullSeq
    let subListsGreaterThanLen1 = filter (\list -> length list > 1) possibleSubLists
    let subListsThatSumToSolution1 = map snd $ filter (\(total, list) -> total == p1Solution) $ map (\list -> (sum list, list)) subListsGreaterThanLen1
    let firstSubListSumingToSolution1 = head subListsThatSumToSolution1
    print $ minimum firstSubListSumingToSolution1 + maximum firstSubListSumingToSolution1

readInput :: IO XmasData 
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let allLines = lines contents
    let nums = map (\l -> read l :: Int) allLines
    return $ createXmasData nums

allSubLists :: [a] -> [[a]]
allSubLists ls = map (\(begin, end) -> slice begin end ls) (allSubListRanges ls)

slice :: Int -> Int -> [a] -> [a]
slice begin end = take (end - begin) . drop begin

allSubListRanges :: [a] -> [(Int, Int)]
allSubListRanges ls = [(b, e) | b <- [0..lastIndex], e <- [lastIndex, (lastIndex - 1)..0], b < e]
    where 
        lastIndex = length ls
