module Seat where

import Text.ParserCombinators.Parsec

type Seat = (String, String)

difference :: Int -> Int -> Int
difference min max = ceiling (fromIntegral (max - min) / 2)

rowNumber :: String -> Int -> Int -> Int
rowNumber [] min max = if min == max then min else error $ "row did not resolve to a single number " ++ show min ++ " " ++ show max
rowNumber ('B' : xs) min max = rowNumber xs (min + difference min max) max
rowNumber ('F' : xs) min max = rowNumber xs min (max - difference min max)

columnNumber :: String -> Int -> Int -> Int
columnNumber [] min max = if min == max then min else error $ "column did not resolve to a single number " ++ show min ++ " " ++ show max
columnNumber ('R' : xs) min max = columnNumber xs (min + difference min max) max
columnNumber ('L' : xs) min max = columnNumber xs min (max - difference min max)

seatId :: Seat -> Int
seatId (row, column) = (rowNum * 8) + colNum
    where 
        rowNum = rowNumber row 0 127
        colNum = columnNumber column 0 7


-- parsing  

seatParser :: GenParser Char st Seat
seatParser = do
    row <- count 7 (oneOf "FB")
    column <- count 3 (oneOf "LR")
    return (row, column)

seatsParser :: GenParser Char st [Seat]
seatsParser = sepBy seatParser newline

parseSeats :: [Char] -> Either ParseError [Seat]
parseSeats input = parse  seatsParser "Error parsing" input

