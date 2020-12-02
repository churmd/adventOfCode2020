module Lib
    ( solve, readInput, getAllPairs, entriesThatMatch2020, pairMatchesSum
    ) where

import Data.List

solve:: IO()
solve = do
    numbers <- readInput
    let (a, b) = entriesThatMatch2020 numbers
    print (a * b)

readInput :: IO[Integer]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    let numbers = lines contents
    return $ map (read::String->Integer) numbers

getAllPairs :: [a] -> [(a, a)]
getAllPairs xs = getAllPairsHelper xs []

getAllPairsHelper :: [a] -> [(a, a)] -> [(a, a)]
getAllPairsHelper [] acc = acc
getAllPairsHelper (x:xs) acc = getAllPairsHelper xs (acc ++ pairFirstElem (x:xs))

pairFirstElem :: [a] -> [(a, a)]
pairFirstElem [] = []
pairFirstElem [x] = []
pairFirstElem (x:xs) = zip (replicate (length xs) x) xs

pairMatchesSum :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
pairMatchesSum xs n = filter (\(a, b) -> a + b == n) xs

entriesThatMatch2020 :: [Integer] -> (Integer, Integer)
entriesThatMatch2020 xs = sum2020
    where
        pairs = getAllPairs xs
        sum2020 = head $ pairMatchesSum pairs 2020