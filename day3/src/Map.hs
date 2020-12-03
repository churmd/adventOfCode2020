module Map where

import Text.ParserCombinators.Parsec

type Coord = (Int, Int)
data Point = Empty | Tree deriving (Show, Eq)

data Map = Map {
    width :: Int,
    height :: Int,
    points :: [(Coord, Point)]
} deriving Show


getPoint :: Coord -> Map -> Point
getPoint (x, y) m = case lookup modCoord (points m) of
    Just p -> p
    Nothing -> error ("Coord " ++ show (x,y) ++ " not found in map") 
    where 
        modCoord = (mod x (width m), mod y (height m))

-- parsing 

emptyPoint :: GenParser Char st Point
emptyPoint = do
    char '.'
    return Empty

treePoint :: GenParser Char st Point
treePoint = do
    char '#'
    return Tree

point :: GenParser Char st Point
point = choice [treePoint, emptyPoint]

row :: GenParser Char st [Point]
row = do
    points <- many1 point 
    optional (char '\n')
    return points

rows :: GenParser Char st [[Point]]
rows = many row

parseMap :: [Char] -> Either ParseError Map
parseMap input = case parse rows "Error" input of 
    Left e -> Left e
    Right pointLists -> Right $ Map width height points
        where
            points = addCoords pointLists (0, 0)
            width = 1 + getWidth points
            height = 1 + getHeight points

addCoords :: [[Point]] -> Coord -> [(Coord, Point)]
addCoords [] _ = []
addCoords ([] : pointLists) (_, y) = addCoords pointLists (0, y + 1) 
addCoords (pl : pls) (x, y) = ((x, y), head pl) : addCoords (tail pl : pls) (x + 1, y)

getWidth :: [(Coord, Point)] -> Int
getWidth [] = 0
getWidth xs = maximum $ map (fst . fst) xs

getHeight :: [(Coord, Point)] -> Int
getHeight [] = 0
getHeight xs = maximum $ map (snd . fst) xs