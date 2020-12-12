module Bags where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe

type Bag = String
data BagRule = BagRule {
    bag :: Bag,
    containedBags :: [(Int, Bag)]
} deriving (Show)


rulesThatCanContainBag :: [BagRule] -> Bag -> [(BagRule, Bool)]
rulesThatCanContainBag rules = evalAllRules rules rules

evalAllRules :: [BagRule] -> [BagRule] -> Bag -> [(BagRule, Bool)]
evalAllRules [] _ _ = []
evalAllRules (r : rs) allRules bag = (r, evalRule r allRules bag) : evalAllRules rs allRules bag

evalRule :: BagRule -> [BagRule] -> Bag -> Bool
evalRule (BagRule _ []) _ _ = False
evalRule (BagRule r subBags) allRules bag 
    | r == bag = False
    | otherwise = directlyHoldBag || or evalSubRules
    where 
        subBagsNames = map snd subBags
        directlyHoldBag = bag `elem` subBagsNames
        findRule bagName = find (\(BagRule name _) -> name == bagName) allRules
        findAllRules = mapMaybe findRule subBagsNames
        evalSubRules = map (\sr -> evalRule sr allRules bag) findAllRules

numOfSubBags :: BagRule -> [BagRule] -> Int 
numOfSubBags (BagRule _ []) _ = 0
numOfSubBags (BagRule _ containedBags) allRules = countSubBags containedBags allRules

countSubBags :: [(Int, Bag)] -> [BagRule] -> Int 
countSubBags [] _ = 0
countSubBags ((count, bagName) : subBags) allRules = 
    case find (\(BagRule name _) -> name == bagName) allRules of 
        Nothing -> error $ "could not find rule for bag " ++ bagName
        Just r -> count + (count * numOfSubBags r allRules) + countSubBags subBags allRules

-- parsing  

parseNumBags :: GenParser Char st (Int, Bag)
parseNumBags = do
    optional space
    num <- many1 digit
    let bagNum = read num :: Int
    space
    bagType <- manyTill anyChar (choice [try (string " bags"), try (string " bag")])
    return (bagNum, bagType)

parseContainingBags :: GenParser Char st [(Int, Bag)]
parseContainingBags = do 
    noOtherBags <- optionMaybe (string "no other bags")
    case noOtherBags of
        Nothing -> sepBy parseNumBags (char ',')
        Just _ -> return []

parseRule :: GenParser Char st BagRule
parseRule = do
    optional newline
    bagType <- manyTill anyChar (try (string " bags contain "))
    containingBags <- parseContainingBags
    return (BagRule bagType containingBags)

parseLines :: GenParser Char st [BagRule]
parseLines = endBy parseRule (char '.')

parseBagRules :: [Char] -> Either ParseError [BagRule]
parseBagRules input = parse  parseLines "Error parsing" input
