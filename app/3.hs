import System.IO
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
    handle <- openFile "data/input3.txt" ReadMode
    lineOne <- hGetLine handle
    lineTwo <- hGetLine handle
    let wireOneInput = splitOn "," lineOne
        wireTwoInput = splitOn "," lineTwo
        wireOnePoints = reverse $ generatePoints wireOneInput [(0,0)]
        wireTwoPoints = reverse $ generatePoints wireTwoInput [(0,0)]
        intersectionDistancesSteps = findIntersectionDistances 
            wireOnePoints wireTwoPoints wireTwoPoints 0 0 ([], [])
    putStrLn $ "Closest Manhattan distance to a crossing of wires: " ++ 
            show (foldr1 min $ fst intersectionDistancesSteps)
    putStrLn $ "Closest number of steps to a crossing of wires: " ++ 
            show (foldr1 min $ snd intersectionDistancesSteps)

generatePoints :: [String] -> [(Int, Int)] -> [(Int, Int)]
generatePoints (x:xs) acc
    | null xs = newAcc
    | otherwise = generatePoints (xs) newAcc
    where 
        newPoint = add x $ head acc
        newAcc = newPoint:acc
    
add :: String -> (Int, Int) -> (Int, Int)
add moveString (x, y)
    | direction == 'L' = (x - toMove, y)
    | direction == 'R' = (x + toMove, y)
    | direction == 'U' = (x, y + toMove)
    | direction == 'D' = (x, y - toMove)
    where
        direction = head moveString
        toMove = read $ tail moveString :: Int

checkIntersect :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Float, Float)
checkIntersect (x1, y1) (x2, y2) (x3, y3) (x4, y4)
    | denominator /= 0 && t >= 0 && t <= 1 && u >= 0 && u <= 1 
        = Just ((fromIntegral x1) + t*(fromIntegral (x2 - x1)), 
                (fromIntegral y1) + t*(fromIntegral (y2 - y1)))
    | otherwise = Nothing
    where
        denominator = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4) 
        t = (fromIntegral ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4))) / 
            (fromIntegral denominator) :: Float
        u = (fromIntegral (-((x1 - x2)*(y1 - y3) - (y1 - y2)*(x1 - x3)))) / 
            (fromIntegral denominator) :: Float
            
findIntersectionDistances :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> ([Int], [Int])-> ([Int], [Int])
findIntersectionDistances (x:[]) _ _ _ _ accDistances = accDistances
findIntersectionDistances (wOneNext:wOneNextNext:wOneRemaining) 
                          (wTwoNext:wTwoNextNext:wTwoRemaining) 
                          wTwoCopy wOneSteps wTwoSteps (distAcc, stepsAcc)
    | null wTwoRemaining && intersects && nextDistNotZero = 
        findIntersectionDistances (wOneNextNext:wOneRemaining) 
            wTwoCopy wTwoCopy (wOneSteps + nextStepWOneToInt) 0 
            (nextDistance:distAcc, bothWSteps:stepsAcc)
    | null wTwoRemaining = findIntersectionDistances 
        (wOneNextNext:wOneRemaining) wTwoCopy wTwoCopy (wOneSteps + nextStepsWOne) 0 (distAcc, stepsAcc)
    | intersects && nextDistNotZero = findIntersectionDistances 
        (wOneNext:wOneNextNext:wOneRemaining) (wTwoNextNext:wTwoRemaining)
        wTwoCopy wOneSteps (wTwoSteps + nextStepWTwoToInt)
        (nextDistance:distAcc, bothWSteps:stepsAcc)
    | otherwise = findIntersectionDistances 
        (wOneNext:wOneNextNext:wOneRemaining) (wTwoNextNext:wTwoRemaining) 
        wTwoCopy wOneSteps (wTwoSteps + nextStepsWTwo) (distAcc, stepsAcc)
    where
        maybeIntersectPoint = checkIntersect wOneNext wOneNextNext wTwoNext wTwoNextNext
        intersects = isJust maybeIntersectPoint
        intersectPoint = fromJust maybeIntersectPoint
        nextDistance = round ((abs (fst intersectPoint)) + (abs (snd intersectPoint)))
        nextDistNotZero = nextDistance > 0
        nextStepsWOne = abs $(abs ((fst wOneNext) - (fst wOneNextNext))) + 
                        (abs ((snd wOneNext) - (snd wOneNextNext)))
        nextStepsWTwo = abs $(abs ((fst wTwoNext) - (fst wTwoNextNext))) + 
                        (abs ((snd wTwoNext) - (snd wTwoNextNext)))
        nextStepWOneToInt = abs $(abs ((fst wOneNext) - (round $ fst intersectPoint))) + 
                            (abs ((snd wOneNext) - (round $ snd intersectPoint)))
        nextStepWTwoToInt = abs $(abs ((fst wTwoNext) - (round $ fst intersectPoint))) + 
                            (abs ((snd wTwoNext) - (round $ snd intersectPoint)))
        bothWSteps = wOneSteps + wTwoSteps + nextStepWOneToInt + nextStepWTwoToInt