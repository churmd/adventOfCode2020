module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Problem 1"
    putStr "number of valid passwords: "
    solve1
    putStrLn "Problem 2"
    putStr "number of valid passwords: "
    solve2
