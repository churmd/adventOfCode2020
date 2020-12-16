module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Day 9"
    putStrLn "problem 1 solution"
    numBreakingSeq <- solve1
    print numBreakingSeq
    putStrLn "problem 2 solution"
    solve2 numBreakingSeq
