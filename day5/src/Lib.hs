module Lib where

import Seat 
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

solve1 :: IO ()
solve1 = do
    seats <- readInput
    let seatIDs = map seatId seats
    let maxID = maximum seatIDs
    print maxID

solve2 :: IO ()
solve2 = do
    seats <- readInput
    let seatIDs = sort $ map seatId seats
    print $ findMySeat seatIDs


readInput :: IO [Seat]
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseSeats contents of
        Left e -> error $ show e
        Right seats -> return seats


findMySeat :: [Int] -> Int
findMySeat (n : m : xs) = if m /= n+1 then n+1 else findMySeat (m : xs)
findMySeat _ = error "no seat found"