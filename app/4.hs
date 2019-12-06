import System.IO

main :: IO ()
main = do
    putStrLn "Enter the lower range of the password: "
    lowerInput <- getLine
    putStrLn "Enter the upper range of the password: "
    upperInput <- getLine
    putStrLn $ "Lower: " ++ lowerInput ++ " upper: " ++ upperInput
    let lower = read lowerInput :: Int
        upper = read upperInput :: Int
        possiblePwds = calcPossPwds lower upper []
    putStrLn $ "Number of possible passwords: " ++ show (length possiblePwds)

calcPossPwds :: Int -> Int -> [Int] -> [Int]
calcPossPwds lower upper possAcc
    | lower == upper + 1 = possAcc
    | isCorrectPswd lower (-1) False = calcPossPwds (lower + 1) upper (lower:possAcc)
    | otherwise = calcPossPwds (lower + 1) upper possAcc

isCorrectPswd :: Int -> Int -> Bool -> Bool
isCorrectPswd password lastEqual correct
    | password < 10 = correct
    | thisNum == nextNum && nextNum == lastEqual = 
        isCorrectPswd (password `div` 10) lastEqual correct
    | thisNum == nextNum && nextNum /= nextNextNum = isCorrectPswd (password `div` 10) (-1) True
    | thisNum == nextNum = isCorrectPswd (password `div` 10) thisNum correct
    | thisNum <= nextNum = False
    | otherwise = isCorrectPswd (password `div` 10) (-1) correct
    where 
        thisNum = password `mod` 10
        nextNum = password `div` 10 `mod` 10
        nextNextNum = password `div` 100 `mod` 10



    