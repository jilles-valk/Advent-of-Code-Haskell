{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Concurrent
import qualified Data.Vector as Vec

main :: IO ()
main = do 
    handle <- openFile "data/input13.txt" ReadMode
    contents <- hGetContents handle
    let program = Vec.fromList $ map read $ splitOn "," contents :: Vec.Vector Int
    programIn <- newEmptyMVar
    programOut <- newMVar []
    waitingForInput <- newMVar False
    forkIO $ compute (program Vec.++ Vec.replicate 1000 0) 0 0 programIn programOut waitingForInput
    threadDelay (3000 * 1000)
    bricks <- readMVar programOut
    putStrLn $ "The number of blocks at the start is: " ++ show (countTypeObject bricks 2 0)
    putStrLn "Computing final score, this might take a while..."
    finalScore <- playGame programIn programOut waitingForInput 18 0 0
    putStrLn $ "The final score: " ++ show finalScore

playGame :: MVar Int -> MVar [Int] -> MVar Bool -> Int -> Int -> Int-> IO (Int)
playGame computerIn computerOut waitingForInput paddlePos score waitingTime = do
    needsInput <- swapMVar waitingForInput False
    if not needsInput then do 
        threadDelay (100)
        if waitingTime > 1000 then do 
            cOut <- takeMVar computerOut
            let (_, _, newScore) = playTurn cOut paddlePos (0, 0, 0)
            return newScore
        else (playGame computerIn computerOut waitingForInput paddlePos score (waitingTime + 1))
    else do
        cOut <- takeMVar computerOut
        putMVar computerOut []
        let (paddleChange, ballPos, newScore) = playTurn cOut paddlePos (0, 0, 0)
            newPaddlePos = paddlePos + paddleChange
        putMVar computerIn paddleChange
        if newScore > score then playGame computerIn computerOut waitingForInput newPaddlePos newScore 0
        else playGame computerIn computerOut waitingForInput newPaddlePos score 0

countTypeObject :: [Int] -> Int -> Int -> Int
countTypeObject [] _ count = count
countTypeObject (kind:_:_:instructions) isKind count = if isKind == kind 
    then countTypeObject instructions isKind (count + 1) 
    else countTypeObject instructions isKind count 
        
playTurn :: [Int] -> Int -> (Int, Int, Int) -> (Int, Int, Int)
playTurn [] _ output = output
playTurn [_] _ output = output
playTurn [_, _] _ output = output
playTurn (kind:y:x:blocks) paddlePos output = do 
    let (oldJoystickPos, oldBallPos, oldScore) = output
        (newJoystickPos, newBallPos) = if oldJoystickPos == 0 && kind == 4 then 
                case compare x paddlePos of
                    LT -> (-1, x)
                    EQ -> (0, x)
                    GT -> (1, x)
            else (oldJoystickPos, oldBallPos)
        newScore = if oldScore == 0 && x == (-1) && y == 0 then kind else oldScore
    playTurn blocks paddlePos (newJoystickPos, newBallPos, newScore)

compute :: Vec.Vector Int -> Int -> Int -> MVar Int -> MVar [Int] -> MVar Bool -> IO ()
compute program index relBIndex input output waitingForInput     
    | opcode == 99 = do 
        putStrLn "Done computing"
    | opcode == 1 = compute (replaceNWithMode program (instructions Vec.! 3) 
                        add relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 2 = compute (replaceNWithMode program (instructions Vec.! 3) 
                        multiply relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 3 = do
        waiting <- isEmptyMVar input
        swapMVar waitingForInput ( if waiting then True else False)
        inputFromMVar <- takeMVar input
        compute (replaceNWithMode program (instructions Vec.! 1) inputFromMVar 
            relBIndex paraOneMode) endIndex relBIndex input output waitingForInput
     | opcode == 4 = do
        modifyMVar_ output (\ oldVal -> return ((takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode):oldVal))
        compute program endIndex relBIndex input output waitingForInput
    | opcode == 5 = if (jumpVal /= 0) 
                        then compute program jumpToIndex relBIndex input output waitingForInput
                        else compute program endIndex relBIndex input output waitingForInput
    | opcode == 6 = if (jumpVal  == 0) 
                        then compute program jumpToIndex  relBIndex input output waitingForInput
                        else compute program endIndex relBIndex input output waitingForInput
    | opcode == 7 = compute (replaceNWithMode program (instructions Vec.! 3) lessThan 
                        relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 8 = compute (replaceNWithMode program (instructions Vec.! 3) equals relBIndex paraThreeMode) 
                        endIndex relBIndex input output waitingForInput
    | opcode == 9 = compute program endIndex (relBIndex + 
                        (takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode)) input output waitingForInput
    where 
        programWODone = Vec.drop index program
        paraOpt = Vec.head programWODone
        opcode = paraOpt `mod` 100
        paraOneMode = paraOpt `div` 100 `mod` 10
        paraTwoMode = paraOpt `div` 1000 `mod` 10
        paraThreeMode = paraOpt `div` 10000 `mod` 10
        numParas = setNumParas opcode
        endIndex = index + numParas + 1
        instructions = Vec.take (numParas + 1) programWODone 
        add = (takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode) + 
              (takeWithMode program (instructions Vec.! 2) relBIndex paraTwoMode)
        multiply = (takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode) * 
                   (takeWithMode program (instructions Vec.! 2) relBIndex paraTwoMode)
        jumpVal = (takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode)
        jumpToIndex = (takeWithMode program (instructions Vec.! 2) relBIndex paraTwoMode)
        lessThan = if ((takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode) < 
                       (takeWithMode program (instructions Vec.! 2) relBIndex paraTwoMode)) then 1 else 0
        equals = if ((takeWithMode program (instructions Vec.! 1) relBIndex paraOneMode) ==
                      (takeWithMode program (instructions Vec.! 2) relBIndex paraTwoMode)) then 1 else 0
        
takeWithMode :: Vec.Vector Int -> Int -> Int -> Int -> Int
takeWithMode program index relativeBaseIndex paraMode 
    | paraMode == 0 = program Vec.! index
    | paraMode == 1 = index
    | paraMode == 2 = program Vec.! (index + relativeBaseIndex)

replaceNWithMode :: Vec.Vector Int -> Int -> Int -> Int -> Int -> Vec.Vector Int
replaceNWithMode vector index newVal relativeBaseIndex paraMode
    | paraMode == 0 = vector Vec.// [(index, newVal)]
    | paraMode == 1 = vector Vec.// [(index, newVal)]
    | paraMode == 2 = vector Vec.// [((index + relativeBaseIndex), newVal)]


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