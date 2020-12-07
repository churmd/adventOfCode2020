module Passport where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Char

type FieldValue = (String, String)
type Passport = [FieldValue]

type Rule = Passport -> Bool

requiredFields :: [String]
requiredFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport = fields == requiredFields
    where
        fields = sort $ filter (`elem` requiredFields) $ map fst passport

numberBetweenInc :: String -> Int -> Int -> Bool
numberBetweenInc val min max = n >= min && n <= max
    where
        n = read val :: Int

baseLookUpRule :: Passport ->  String -> (String -> Bool) -> Bool
baseLookUpRule passport field r = maybe False r (lookup field passport)

byrRule :: Rule
byrRule passport = baseLookUpRule passport "byr" (\val -> length val == 4 && numberBetweenInc val 1920 2002)

iyrRule :: Rule
iyrRule passport = baseLookUpRule passport "iyr" (\val -> length val == 4 && numberBetweenInc val 2010 2020)

eyrRule :: Rule
eyrRule passport = baseLookUpRule passport "eyr" (\val -> length val == 4 && numberBetweenInc val 2020 2030)

hgtRule :: Rule
hgtRule passport = baseLookUpRule passport "hgt" rule 
    where
        rule val 
            | "cm" `isSuffixOf` val = numberBetweenInc (takeWhile (/= 'c') val) 150 193
            | "in" `isSuffixOf` val = numberBetweenInc (takeWhile (/= 'i') val) 59 76
            | otherwise = False

hclRule :: Rule
hclRule passport = baseLookUpRule passport "hcl" rule
    where   
        rule ('#' : xs) = all isHexDigit xs
        rule _ = False

eclRule :: Rule
eclRule passport = baseLookUpRule passport "ecl" (\val -> val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

pidRule :: Rule
pidRule passport = baseLookUpRule passport "pid" (\val -> length val == 9 && all isDigit val)

passesAllRules :: Passport -> Bool
passesAllRules passport = all (\ r -> r passport) rules
    where 
        rules = [byrRule, iyrRule, eyrRule, hgtRule, hclRule, eclRule, pidRule]

-- parsing

emptyLine :: GenParser Char st String
emptyLine = try (string "\n\n")

passportFieldValue :: GenParser Char st FieldValue
passportFieldValue = do
    optional space
    optional newline
    name <- many1 (noneOf ":")
    char ':'
    value <- many1 (noneOf " \n")
    return (name, value)

passportText:: GenParser Char st [FieldValue]
passportText = manyTill passportFieldValue emptyLine

passports :: GenParser Char st [Passport]
passports = many passportText

allText :: GenParser Char st String
allText = manyTill anyToken eof

parsePassports :: [Char] -> Either ParseError [Passport]
parsePassports input = parse passports "Error parsing" (input ++ "\n\n")