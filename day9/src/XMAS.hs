module XMAS where 

import Data.List

type XmasData = ([Int], [Int])

createXmasData :: [Int] -> XmasData
createXmasData numSeq = if length numSeq >= 25 then splitAt 25 numSeq else error "XMAS number sequence is too short to conatin a preamble"

xmasDataLen :: XmasData -> Int 
xmasDataLen (preamble, numSeq) = length (preamble ++ numSeq)

get25NumsBeforeIndex :: XmasData -> Int -> ([Int], Int)
get25NumsBeforeIndex (pre, nums) index 
    | index < 25 = error "Cannot get 25 numbers before index as it is still in the preamble"
    | index >= length wholeSeq = error "index is larger than the sequence of numbers"
    | otherwise = (leading25Nums, head indexOnwards)
    where 
        wholeSeq = pre ++ nums 
        (before, indexOnwards) = splitAt index wholeSeq
        (_, leading25Nums) = splitAt (length before - 25) before

getAllPairings :: (Eq a) => [a] -> [(a, a)]
getAllPairings ls = [(x, y) | x <- ls, y <- delete x ls]

anyPairingsSumTo :: [(Int, Int)] -> Int -> Bool 
anyPairingsSumTo [] _ = False 
anyPairingsSumTo ((n1, n2) : xs) val = (n1 + n2 == val) || anyPairingsSumTo xs val