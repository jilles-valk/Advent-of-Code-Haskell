import System.IO
import Data.List.Split
import Data.List
import Data.Maybe

main :: IO ()
main = do 
    handle <- openFile "data/input10.txt" ReadMode
    contents <- hGetContents handle
    let rawMap = splitOn "\r\n" contents
        astMap = map fromJust $ filter isJust $ concat $ concat $ 
                    mapAccumL (\y row -> (y + 1, snd (mapAccumL 
                    (\x c -> (x + 1, makeCoordinate x y c)) 0 row))) 0 rawMap
        (numAstroids, bestStation) = findBestMonitoringStation astMap astMap (-1, -1) (-1)
        destroyedAst = reverse $ searchAndDestroy astMap [] bestStation
        twoHundredth = destroyedAst !! 199
    putStrLn "The input map is: "
    traverse putStrLn rawMap
    putStrLn $ "The best station is at: " ++ show bestStation ++ " with " 
        ++ show numAstroids ++ " astroids visible." 
    putStrLn $ "Output for 200th destroyed astroid from best station is: " 
        ++ show ((fst twoHundredth)*100 + (snd twoHundredth))
    hClose handle

makeCoordinate :: Int -> Int -> Char -> Maybe (Int, Int)
makeCoordinate x y c
    | c == '#' = Just (x,y)
    | otherwise = Nothing

searchAndDestroy :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
searchAndDestroy allAstroids destroyedAst monStat
    | null visibleAstroids = destroyedAst
    | otherwise = searchAndDestroy (allAstroids \\ sortedVisAst) 
        (sortedVisAst ++ destroyedAst) monStat
    where
        visibleAstroids = findDetectableAstroids allAstroids allAstroids monStat []
        sortedVisAst = sortOn (\ast -> getAngleToAst monStat ast) visibleAstroids

findBestMonitoringStation :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int -> (Int, (Int, Int))
findBestMonitoringStation _ [] bestStat numAst = (numAst, bestStat)
findBestMonitoringStation allAstroids (thisAst:otherAst) bestStat numAst
    | numDetectAstThis > numAst = findBestMonitoringStation 
                                allAstroids otherAst thisAst numDetectAstThis
    | otherwise = findBestMonitoringStation 
                    allAstroids otherAst bestStat numAst
    where 
        numDetectAstThis = length $ findDetectableAstroids allAstroids allAstroids thisAst [] 

findDetectableAstroids :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findDetectableAstroids _ [] _ detAst = detAst 
findDetectableAstroids allAstroids (thisAst:otherAst) monStat detAst
    | isBlocked allAstroids thisAst monStat = 
        findDetectableAstroids allAstroids otherAst monStat detAst
    | otherwise = findDetectableAstroids allAstroids otherAst monStat (thisAst:detAst)

isBlocked :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Bool
isBlocked (thisAst:otherAst) astroidA astroidB
    | null otherAst && not aIsB = False
    | null otherAst && aIsB = True
    | isJust angAThis && isJust angAB && abThisOnLine && isThisAstBetween = True
    | otherwise = isBlocked otherAst astroidA astroidB 
    where 
        aIsB = astroidA == astroidB
        thisNotAOrB = thisAst == astroidA || thisAst == astroidB
        angAThis= getAngleToAst astroidA thisAst
        angAB = getAngleToAst astroidA astroidB
        abThisOnLine = (abs (fromJust angAThis - fromJust angAB)) < 0.001
        distAB = getDist astroidA astroidB
        isThisAstBetween = getDist thisAst astroidA < distAB && 
                           getDist thisAst astroidB < distAB

getAngleToAst :: (Int, Int) -> (Int, Int) -> Maybe Float
getAngleToAst (xa, ya) (xb, yb) 
    | not deltaYZero = Just $ atan2 (fromIntegral (xb - xa)) (fromIntegral (yb - ya))
    | deltaYZero = if xa < xb then Just (0.5*pi) else Just 0
    | otherwise = Nothing
    where 
        deltaYZero = ya == yb
        
getDist :: (Int, Int) -> (Int, Int) -> Float
getDist (xa, ya) (xb, yb) = sqrt (fromIntegral ((xb - xa)^2 + (yb - ya)^2))
