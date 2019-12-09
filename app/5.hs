import System.IO
import Data.List.Split

main :: IO ()
main = do 
    handle <- openFile "data/input5.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
    putStrLn $ "The output is: " 
        ++ show (compute program 0 [5] [])

compute :: [Int] -> Int -> [Int] -> [Int] -> [Int]
compute program index input output
    | length program < index = output
    | opcode == 99 = output
    | opcode == 1 = compute (replaceN program (instructions !! 3) add) 
                        endIndex input output
    | opcode == 2 = compute (replaceN program (instructions !! 3) multiply) 
                        endIndex input output
    | opcode == 3 = compute (replaceN program (instructions !! 1) (head input)) 
                        endIndex (tail input) output
    | opcode == 4 = compute program endIndex input 
                        ((takeWithMode program (instructions !! 1) paraOneMode):output)
    | opcode == 5 = if (jumpVal /= 0) 
                        then compute program jumpToIndex input output
                        else compute program endIndex input output
    | opcode == 6 = if (jumpVal  == 0) 
                        then compute program jumpToIndex  input output
                        else compute program endIndex input output
    | opcode == 7 = compute (replaceN program (instructions !!3) lessThan) 
                        endIndex input output
    | opcode == 8 = compute (replaceN program (instructions !!3) equals) 
                        endIndex input output
    | otherwise = [-1]
    where 
        programWODone = drop index program
        paraOpt = head programWODone
        opcode = paraOpt `mod` 100
        paraOneMode = paraOpt `div` 100 `mod` 10
        paraTwoMode = paraOpt `div` 1000 `mod` 10
        paraThreeMode = paraOpt `div` 10000 `mod` 10
        numParas = setNumParas opcode
        endIndex = index + numParas + 1
        instructions = take (numParas + 1) programWODone 
        add = (takeWithMode program (instructions !! 1) paraOneMode) + 
              (takeWithMode program (instructions !! 2) paraTwoMode)
        multiply = (takeWithMode program (instructions !! 1) paraOneMode) * 
                   (takeWithMode program (instructions !! 2) paraTwoMode)
        jumpVal = (takeWithMode program (instructions !! 1) paraOneMode)
        jumpToIndex = (takeWithMode program (instructions !! 2) paraTwoMode)
        lessThan = if ((takeWithMode program (instructions !! 1) paraOneMode) < 
                       (takeWithMode program (instructions !! 2) paraTwoMode)) then 1 else 0
        equals = if ((takeWithMode program (instructions !! 1) paraOneMode) ==
                      (takeWithMode program (instructions !! 2) paraTwoMode)) then 1 else 0
        
takeWithMode :: [Int] -> Int -> Int -> Int
takeWithMode program index paraMode 
    | paraMode == 0 = program !! index
    | paraMode == 1 = index

replaceN :: [Int] -> Int -> Int -> [Int]
replaceN (x:xs) n newVal
    | n == 0 = newVal:xs
    | otherwise = x: replaceN xs (n-1) newVal

setNumParas :: Int -> Int
setNumParas opcode
    | opcode == 99 = 0
    | opcode == 1 = 3
    | opcode == 2 = 3
    | opcode == 3 = 1
    | opcode == 4 = 1
    | opcode == 5 = 2
    | opcode == 6 = 2
    | opcode == 7 = 3
    | opcode == 8 = 3
    