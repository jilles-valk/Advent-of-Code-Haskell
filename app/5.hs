import System.IO
import Data.List.Split

main :: IO ()
main = do 
    handle <- openFile "data/input5.txt" ReadMode
    contents <- hGetContents handle
    let input = map read $ splitOn "," contents :: [Int]
    putStrLn $ "The output is: " 
        ++ show (compute input 0)

compute :: [Int] -> Int -> Int
compute input index
    | length input < index = head input
    | opcode == 99 = head input
    | opcode == 1 = compute (replaceN input (instructions !! 3) add) endIndex
    | opcode == 2 = compute (replaceN input (instructions !! 3) multiply) endIndex
    | opcode == 3 = compute (replaceN input (instructions !! 1) 1) endIndex
    | opcode == 4 = input !! (instructions !! 1)
    | otherwise = -1
    where 
        inputWODone = drop index input
        paraOpt = head inputWODone
        opcode = paraOpt `mod` 100
        paraOneMode = paraOpt `div` 100 `mod` 10
        paraTwoMode = paraOpt `div` 1000 `mod` 10
        paraThreeMode = paraOpt `div` 10000 `mod` 10
        numParas = case opcode of 
                    99 -> 0
                    1 -> 3
                    2 -> 3
                    3 -> 1
                    4 -> 1
        endIndex = index + numParas + 1
        instructions = take (numParas + 1) inputWODone 
        add = (takeWithMode input (instructions !! 1) paraOneMode) + 
              (takeWithMode input (instructions !! 2) paraTwoMode)
        multiply = (takeWithMode input (instructions !! 1) paraOneMode) * 
                   (takeWithMode input (instructions !! 2) paraTwoMode)
        
takeWithMode :: [Int] -> Int -> Int -> Int
takeWithMode input index paraMode 
    | paraMode == 0 = input !! index
    | paraMode == 1 = index
    -- | otherwise = -1

replaceN :: [Int] -> Int -> Int -> [Int]
replaceN (x:xs) n newVal
    | n == 0 = newVal:xs
    | otherwise = x: replaceN xs (n-1) newVal

