import System.IO
import Data.List.Split
import Data.List (permutations)
import Control.Concurrent

main :: IO ()
main = do 
    handle <- openFile "data/input7.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
        possCombis = permutations $ generateArray [] 5
        possCombisLoop = permutations $ generateArray [9] 5
        maxThrustSignal = largestOutput program possCombis 0
    putStrLn $ "The maximum possible signal for the thrusters is (serial): " ++ 
         show maxThrustSignal
    parallelLargestOutputResult <- parallelLargestOutput program possCombisLoop 0
    putStrLn $ "The maximum possible signal for the thrusters is (loop): " ++ 
        show parallelLargestOutputResult

parallelLargestOutput :: [Int] -> [[Int]] -> Int -> IO Int
parallelLargestOutput program (combi:possCombis) output
    | null possCombis = do
        combiResult <- tryCombiParallel program combi
        return $ if combiResult > output then combiResult else output
    | otherwise = do
        combiResult <- tryCombiParallel program combi
        if combiResult > output then 
            parallelLargestOutput program possCombis combiResult
        else 
            parallelLargestOutput program possCombis output

tryCombiParallel :: [Int] -> [Int] -> IO (Int)
tryCombiParallel program combi = do 
    ampAOut <- newEmptyMVar
    ampBOut <- newEmptyMVar
    ampCOut <- newEmptyMVar
    ampDOut <- newEmptyMVar
    ampEOut <- newEmptyMVar
    ampAResult <- newEmptyMVar
    ampBResult <- newEmptyMVar
    ampCResult <- newEmptyMVar
    ampDResult <- newEmptyMVar
    ampEResult <- newEmptyMVar
    forkIO $ computeParallel program 0 [combi !! 0, 0] ampAOut ampEOut ampAResult
    forkIO $ computeParallel program 0 [combi !! 1] ampBOut ampAOut ampBResult
    forkIO $ computeParallel program 0 [combi !! 2] ampCOut ampBOut ampCResult
    forkIO $ computeParallel program 0 [combi !! 3] ampDOut ampCOut ampDResult
    forkIO $ computeParallel program 0 [combi !! 4] ampEOut ampDOut ampEResult
    result <- takeMVar ampEResult
    return result

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

computeParallel :: [Int] -> Int -> [Int] -> MVar Int -> MVar Int -> MVar Int -> IO ()
computeParallel program index input thisMVar prevMVar resultMVar
    | opcode == 99 = do 
        thisResult <- takeMVar thisMVar
        putMVar resultMVar thisResult
    | opcode == 1 = computeParallel (replaceN program (instructions !! 3) add) 
                        endIndex input thisMVar prevMVar resultMVar
    | opcode == 2 = computeParallel (replaceN program (instructions !! 3) multiply) 
                        endIndex input thisMVar prevMVar resultMVar
    | opcode == 3 = do
        if null input then do
            inputFromMVar <- takeMVar prevMVar
            computeParallel (replaceN program (instructions !! 1) inputFromMVar) 
                endIndex input thisMVar prevMVar resultMVar
        else 
            computeParallel (replaceN program (instructions !! 1) (head input)) 
                endIndex (tail input) thisMVar prevMVar resultMVar
    | opcode == 4 = do
        putMVar thisMVar (takeWithMode program (instructions !! 1) paraOneMode)
        computeParallel program endIndex input thisMVar prevMVar resultMVar
    | opcode == 5 = if (jumpVal /= 0) 
                        then computeParallel program jumpToIndex input thisMVar prevMVar resultMVar
                        else computeParallel program endIndex input thisMVar prevMVar resultMVar
    | opcode == 6 = if (jumpVal  == 0) 
                        then computeParallel program jumpToIndex  input thisMVar prevMVar resultMVar
                        else computeParallel program endIndex input thisMVar prevMVar resultMVar
    | opcode == 7 = computeParallel (replaceN program (instructions !!3) lessThan) 
                        endIndex input thisMVar prevMVar resultMVar
    | opcode == 8 = computeParallel (replaceN program (instructions !!3) equals) 
                        endIndex input thisMVar prevMVar resultMVar
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