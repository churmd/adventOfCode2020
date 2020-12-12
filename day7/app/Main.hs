module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Day 7"
    putStrLn "problem 1 solution (very slow, would be better with dynamic programming)"
    solve1
    putStrLn "problem 2 solution"
    solve2
