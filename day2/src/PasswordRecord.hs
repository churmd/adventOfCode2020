module PasswordRecord (PasswordRecord, isValidProblem1, isValidProblem2, parseRules) where

import Text.ParserCombinators.Parsec

data PasswordRecord = PasswordRecord {
  num1 :: Int,
  num2 :: Int,
  letter :: Char,
  password :: String
} deriving (Eq, Show)

isValidProblem1 :: PasswordRecord -> Bool
isValidProblem1 (PasswordRecord min max letter password) = countOfLetter >= min && countOfLetter <= max
    where 
        countOfLetter = (length . filter (== letter)) password


isValidProblem2 :: PasswordRecord -> Bool
isValidProblem2 (PasswordRecord i1 i2 letter password) = index1Match /= index2Match
    where 
        index1 = i1 - 1
        index2 = i2 - 1
        index1Match = (password!!index1) == letter
        index2Match = (password!!index2) == letter

-- Parsing rules

dash :: GenParser Char st Char
dash = char '-'

colon :: GenParser Char st Char
colon = char ':'

number :: GenParser Char st Int
number = do 
    number <- many1 digit
    return (read number :: Int)

passwordText :: GenParser Char st String
passwordText = many1 (noneOf "\n")

rule :: GenParser Char st PasswordRecord
rule = do
    min <- number
    dash
    max <- number
    space
    letter <- anyChar
    colon
    space
    password <- passwordText
    optional (char '\n')
    return (PasswordRecord min max letter password)


allRules :: GenParser Char st [PasswordRecord]
allRules = many rule

parseRules :: [Char] -> Either ParseError [PasswordRecord]
parseRules  = parse allRules "Error" 