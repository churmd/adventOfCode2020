module Lib ( solve1, solve2) where

solve1:: IO()
solve1 = do
    numbers <- readInput
    let values = entriesThatMatch2020 numbers 2
    print values
    print $ product values

solve2:: IO()
solve2 = do
    numbers <- readInput
    let values = entriesThatMatch2020 numbers 3
    print values
    print $ product values

readInput :: IO[Int]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let numbers = lines contents
    return $ map (read::String->Int) numbers


entriesThatMatch2020 :: [Int] -> Int -> [Int]
entriesThatMatch2020 xs length = head combosSumTo2020
    where
        combos = makeUniqueCombos xs length
        combosSumTo2020 = filter (\c -> sum c == 2020) combos

{-
Build all unique combinations of elements from a list that are n long
e.g. makeUniqueCombos [1,2,3] 2 = [[1,2],[3,2],[1,3],[2,3]]
-}
makeUniqueCombos :: [Int] -> Int -> [[Int]]
makeUniqueCombos [] _ = []
makeUniqueCombos xs 1 = map (: []) xs
makeUniqueCombos options@(_:xs) n = [option : subSeq | subSeq<-subSeqs, option<-options, option `notElem` subSeq]
    where 
        subSeqs = makeUniqueCombos xs (n - 1)