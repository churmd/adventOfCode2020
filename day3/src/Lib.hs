module Lib where
import Map

solve1 :: IO()
solve1 = do
    map <- readInput
    let treesHit = treesHitDownSlope problem1Vector map
    print treesHit

solve2 :: IO()
solve2 = do
    m <- readInput
    let treesHit = map (\coord -> treesHitDownSlope coord m) [(1,1),(3,1),(5,1),(7,1),(1,2)]
    print $ product treesHit

readInput :: IO Map
readInput = do
    let file = "input.txt"
    contents <- readFile file
    case parseMap contents of
        Left e -> error $ show e
        Right map -> return map

problem1Vector :: Coord
problem1Vector = (3, 1)

treesHitDownSlope :: Coord -> Map -> Int
treesHitDownSlope slopeVec m = length $ filter ( == Tree) (goDownSlope slopeVec m (0,0))

goDownSlope :: Coord -> Map -> Coord -> [Point]
goDownSlope slopVec m currentCoord
    | snd currentCoord >= height m = []
    | otherwise = getPoint currentCoord m : goDownSlope slopVec m (fst currentCoord + fst slopVec, snd currentCoord + snd slopVec)