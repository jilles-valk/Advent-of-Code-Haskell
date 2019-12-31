import System.IO
import Data.List.Split

main :: IO ()
main = do 
    handle <- openFile "data/input9.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
        output = reverse $ compute (program ++ [0,0..]) 0 0 [1] []
    putStrLn $ "The output is: " 
        ++ show output

compute :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
compute program index relBIndex input output
    | opcode == 99 = output
    | opcode == 1 = compute (replaceNWithMode program (instructions !! 3) 
                        add relBIndex paraThreeMode) endIndex relBIndex input output
    | opcode == 2 = compute (replaceNWithMode program (instructions !! 3) 
                        multiply relBIndex paraThreeMode) endIndex relBIndex input output
    | opcode == 3 = compute (replaceNWithMode program (instructions !! 1) (head input) 
                        relBIndex paraOneMode) endIndex relBIndex (tail input) output
    | opcode == 4 = compute program endIndex relBIndex input ((takeWithMode program 
                        (instructions !! 1) relBIndex paraOneMode):output)
    | opcode == 5 = if (jumpVal /= 0) 
                        then compute program jumpToIndex relBIndex input output
                        else compute program endIndex relBIndex input output
    | opcode == 6 = if (jumpVal  == 0) 
                        then compute program jumpToIndex  relBIndex input output
                        else compute program endIndex relBIndex input output
    | opcode == 7 = compute (replaceNWithMode program (instructions !!3) lessThan 
                        relBIndex paraThreeMode) endIndex relBIndex input output
    | opcode == 8 = compute (replaceNWithMode program (instructions !!3) equals relBIndex paraThreeMode) 
                        endIndex relBIndex input output
    | opcode == 9 = compute program endIndex (relBIndex + 
                        (takeWithMode program (instructions !! 1) relBIndex paraOneMode)) input output
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
        add = (takeWithMode program (instructions !! 1) relBIndex paraOneMode) + 
              (takeWithMode program (instructions !! 2) relBIndex paraTwoMode)
        multiply = (takeWithMode program (instructions !! 1) relBIndex paraOneMode) * 
                   (takeWithMode program (instructions !! 2) relBIndex paraTwoMode)
        jumpVal = (takeWithMode program (instructions !! 1) relBIndex paraOneMode)
        jumpToIndex = (takeWithMode program (instructions !! 2) relBIndex paraTwoMode)
        lessThan = if ((takeWithMode program (instructions !! 1) relBIndex paraOneMode) < 
                       (takeWithMode program (instructions !! 2) relBIndex paraTwoMode)) then 1 else 0
        equals = if ((takeWithMode program (instructions !! 1) relBIndex paraOneMode) ==
                      (takeWithMode program (instructions !! 2) relBIndex paraTwoMode)) then 1 else 0
        
takeWithMode :: [Int] -> Int -> Int -> Int -> Int
takeWithMode program index relativeBaseIndex paraMode 
    | paraMode == 0 = program !! index
    | paraMode == 1 = index
    | paraMode == 2 = program !! (index + relativeBaseIndex)

replaceNWithMode :: [Int] -> Int -> Int -> Int -> Int -> [Int]
replaceNWithMode list index newVal relativeBaseIndex paraMode
    | paraMode == 0 = replaceN list index newVal
    | paraMode == 1 = replaceN list index newVal
    | paraMode == 2 = replaceN list (index + relativeBaseIndex) newVal

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
    | opcode == 9 = 1
    