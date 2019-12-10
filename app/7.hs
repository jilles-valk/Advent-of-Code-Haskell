import System.IO
import Data.List.Split
import Data.List (permutations)

main :: IO ()
main = do 
    handle <- openFile "data/input7.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
        possCombis = permutations $ generateArray [] 5
        possCombisLoop = permutations $ generateArray [9] 5
        maxThrustSignal = largestOutput program possCombis 0
        maxThrustSignalLoop = largestOutputLoop program possCombisLoop 0
        -- cl9 = computeLoop program 0 [9,0] 0
        -- (cl9Program,cl9Index,cl9Out) = cl9
        -- cl8 = computeLoop program 0 [8, cl9Out] 0
        -- (cl8Program,cl8Index,cl8Out) = cl8
        -- cl7 = computeLoop program 0 [7, cl8Out] 0
        -- (cl7Program,cl7Index,cl7Out) = cl7
        -- cl6 = computeLoop program 0 [6, cl7Out] 0
        -- (cl6Program,cl6Index,cl6Out) = cl6
        -- cl5 = computeLoop program 0 [5, cl6Out] 0
        -- (cl5Program,cl5Index,cl5Out) = cl5

        -- cl91 = computeLoop cl9Program cl9Index [9,11] 0
        -- (cl9Program1,cl9Index1,cl9Out1) = cl91
        -- cl81 = computeLoop cl8Program cl8Index [8, cl9Out1] 0
        -- (cl8Program1,cl8Index1,cl8Out1) = cl81
        -- cl71 = computeLoop cl7Program cl7Index [7, cl8Out1] 0
        -- (cl7Program1,cl7Index1,cl7Out1) = cl71
        -- cl61 = computeLoop cl6Program cl6Index [6, cl7Out1] 0
        -- (cl6Program1,cl6Index1,cl6Out1) = cl61
        -- cl51 = computeLoop cl5Program cl5Index [5, cl6Out1] 0
        -- (cl5Program1,cl5Index1,cl5Out1) = cl51
    putStrLn $ "The maximum possible signal for the thrusters is (serial): " ++ 
         show maxThrustSignal
    putStrLn $ "The maximum possible signal for the thrusters is (loop): " ++ 
         show maxThrustSignalLoop
    -- putStrLn $ show cl9Program ++ " " ++ show cl8Program ++ " " ++ show cl7Out1 ++ " " ++ show cl6Out1 ++ " " ++ show cl5Out1
    -- putStrLn $ show cl9Index1 ++ " " ++ show cl8Index1 ++ " " ++ show cl7Index1 ++ " " ++ show cl6Index1 ++ " " ++ show cl5Index1
    putStrLn $ show $ combiLoop [program, program, program, program, program] [9,7,8,5,6] [0, 0, 0, 0, 0] 0 [[],[],[],[],[]]

generateArray :: [Int] -> Int -> [Int]
generateArray arr len 
    | length arr == len = arr
    | null arr = generateArray [len - 1] len
    | otherwise = generateArray ((head arr - 1):arr) len

largestOutput :: [Int] -> [[Int]] -> Int -> Int
largestOutput program (combi:possCombis) output
    | null possCombis = newOutput
    | otherwise = largestOutput program possCombis newOutput
    where 
        ampA = compute program 0 [combi !! 0, 0] []
        ampB = compute program 0 [combi !! 1, head ampA] []
        ampC = compute program 0 [combi !! 2, head ampB] []
        ampD = compute program 0 [combi !! 3, head ampC] []
        ampE = compute program 0 [combi !! 4, head ampD] []
        newOutput = if (head ampE) > output then head ampE else output 

largestOutputLoop :: [Int] -> [[Int]] -> Int -> Int
largestOutputLoop program (combi:possCombis) output
    | null possCombis = newOutput
    | otherwise = largestOutputLoop program possCombis newOutput
    where 
        combiResult = combiLoop [program, program, program, program, program] combi [0, 0, 0, 0, 0] 0 [[0],[0],[0],[0],[0]]
        newOutput = if head combiResult > output then head combiResult else output        

combiLoop :: [[Int]] -> [Int] -> [Int] -> Int -> [[Int]] -> [Int]
combiLoop programs combi indexes ampAIn outputs
    | ampEindex == (minBound :: Int) = ampE
    | otherwise = combiLoop newPrograms combi newIndexes (head ampE) newOutputs
    where
        (ampAProgram, ampAindex, ampA) = computeLoop (programs !! 0) (indexes !! 0) [combi !! 0, ampAIn] (outputs !! 0)
        (ampBProgram, ampBindex, ampB) = computeLoop (programs !! 1) (indexes !! 1) [combi !! 1, head ampA] (outputs !! 1)
        (ampCProgram, ampCindex, ampC) = computeLoop (programs !! 2) (indexes !! 2) [combi !! 2, head ampB] (outputs !! 2)
        (ampDProgram, ampDindex, ampD) = computeLoop (programs !! 3) (indexes !! 3) [combi !! 3, head ampC] (outputs !! 3)
        (ampEProgram, ampEindex, ampE) = computeLoop (programs !! 4) (indexes !! 4) [combi !! 4, head ampD] (outputs !! 4)
        newIndexes = [ampAindex, ampBindex, ampCindex, ampDindex, ampEindex]
        newPrograms = [ampAProgram, ampBProgram, ampCProgram, ampDProgram, ampEProgram]
        newOutputs = [ampA, ampB, ampC, ampD, ampE]
    
computeLoop :: [Int] -> Int -> [Int] -> [Int] -> ([Int], Int, [Int])
computeLoop program index input output
    | opcode == 99 = (program, minBound :: Int, output)
    | opcode == 1 = computeLoop (replaceN program (instructions !! 3) add) 
                        endIndex input output
    | opcode == 2 = computeLoop (replaceN program (instructions !! 3) multiply) 
                        endIndex input output
    | opcode == 3 = computeLoop (replaceN program (instructions !! 1) (head input)) 
                        endIndex (tail input) output
    | opcode == 4 = (program, endIndex, (takeWithMode program (instructions !! 1) paraOneMode):output)
    | opcode == 5 = if (jumpVal /= 0) 
                        then computeLoop program jumpToIndex input output
                        else computeLoop program endIndex input output
    | opcode == 6 = if (jumpVal  == 0) 
                        then computeLoop program jumpToIndex input output
                        else computeLoop program endIndex input output
    | opcode == 7 = computeLoop (replaceN program (instructions !!3) lessThan) 
                        endIndex input output
    | opcode == 8 = computeLoop (replaceN program (instructions !!3) equals) 
                        endIndex input output
    | otherwise = (program, minBound :: Int, [-1])
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

compute :: [Int] -> Int -> [Int] -> [Int] -> [Int]
compute program index input output
    | length program < index = output
    | opcode == 99 = output
    | opcode == 1 = compute (replaceN program (instructions !! 3) add) 
                        endIndex input output
    | opcode == 2 = compute (replaceN program (instructions !! 3) multiply) 
                        endIndex input output
    | opcode == 3 = if null input then (minBound :: Int):index:output
                    else compute (replaceN program (instructions !! 1) (head input)) 
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
    