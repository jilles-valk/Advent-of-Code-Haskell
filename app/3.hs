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
        wireOnePoints = generatePoints wireOneInput [(0,0)]
        wireTwoPoints = generatePoints wireTwoInput [(0,0)]
        intersectionDistances = findIntersectionDistances wireOnePoints wireTwoPoints wireTwoPoints []
    putStrLn $ "Closest Manhattan distance to the crossing of wires: " ++ show (foldr1 min intersectionDistances)

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
        = Just ((fromIntegral x1) + t*(fromIntegral (x2 - x1)), (fromIntegral y1) + t*(fromIntegral (y2 - y1)))
    | otherwise = Nothing
    where
        denominator = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4) 
        t = (fromIntegral ((x1 - x3)*(y3 - y4) - (y1 - y3)*(x3 - x4))) / (fromIntegral denominator) :: Float
        u = (fromIntegral (-((x1 - x2)*(y1 - y3) - (y1 - y2)*(x1 - x3)))) / (fromIntegral denominator) :: Float
            

findIntersectionDistances :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [Int] -> [Int]
findIntersectionDistances (x:[]) _ _ accDistances = accDistances
findIntersectionDistances (wOneNext:wOneNextNext:wOneRemaining) (wTwoNext:wTwoNextNext:wTwoRemaining) wTwoCopy accDistances
    | null wTwoRemaining && intersects && nextDistNotZero = findIntersectionDistances 
        (wOneNextNext:wOneRemaining) wTwoCopy wTwoCopy (nextDistance:accDistances)
    | null wTwoRemaining = findIntersectionDistances 
        (wOneNextNext:wOneRemaining) wTwoCopy wTwoCopy accDistances
    | intersects && nextDistNotZero= findIntersectionDistances 
        (wOneNext:wOneNextNext:wOneRemaining) (wTwoNextNext:wTwoRemaining) wTwoCopy (nextDistance:accDistances)
    | otherwise = findIntersectionDistances 
        (wOneNext:wOneNextNext:wOneRemaining) (wTwoNextNext:wTwoRemaining) wTwoCopy accDistances
    where
        maybeIntersectPoint = checkIntersect wOneNext wOneNextNext wTwoNext wTwoNextNext
        intersects = isJust maybeIntersectPoint
        intersectPoint = fromJust maybeIntersectPoint
        nextDistance = round ((abs (fst intersectPoint)) + (abs (snd intersectPoint)))
        nextDistNotZero = nextDistance > 0