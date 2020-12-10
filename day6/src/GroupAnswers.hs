module GroupAnswers where

import Data.List

type Answer = Char
type PersonsAnswers = [Answer]
type GroupAnswers = [PersonsAnswers]

getAGroupsUniqueAnswers :: GroupAnswers -> [Answer]
getAGroupsUniqueAnswers ga = nub $ concat ga

answersEveryoneHad :: GroupAnswers -> [Answer]
answersEveryoneHad ga = answersEveryoneHadHelper ga (getAGroupsUniqueAnswers ga)

answersEveryoneHadHelper :: GroupAnswers -> [Answer] -> [Answer]
answersEveryoneHadHelper ps allAnswers
    = foldl intersect allAnswers ps

allGroupsParser :: String -> [GroupAnswers]
allGroupsParser input = groupsParser (lines input) [] []

groupsParser :: [String] -> GroupAnswers -> [GroupAnswers] -> [GroupAnswers]
groupsParser [] accPeople accGroups =  accGroups ++ [accPeople]
groupsParser ([] : ls) accPeople accGroups = groupsParser ls [] (accGroups ++ [accPeople])
groupsParser (l : ls) accPeople accGroups = groupsParser ls (l : accPeople) accGroups



