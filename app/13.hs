{-# LANGUAGE OverloadedStrings #-}
import Graphics.Blank
import System.IO
import Data.List.Split
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Concurrent
-- import Control.Concurrent.Async
import Data.Text as T (pack)

main :: IO ()
main = do 
    handle <- openFile "data/input13.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
    programIn <- newEmptyMVar
    programOut <- newMVar []
    waitingForInput <- newMVar False
    forkIO $ compute (program ++ [0,0..]) 0 0 programIn programOut waitingForInput
    threadDelay (3000 * 1000)
    bricks <- readMVar programOut
    putStrLn $ "The number of blocks is at the start is: " ++ show (countTypeObject bricks 2 0)
    putStrLn "Computing final score, this might take a while..."
    finalScore <- playGame programIn programOut waitingForInput 18 0 0
    putStrLn $ "The final score: " ++ show finalScore

playGame :: MVar Int -> MVar [Int] -> MVar Bool -> Int -> Int -> Int-> IO (Int)
playGame computerIn computerOut waitingForInput paddlePos score waitingTime = do
    needsInput <- swapMVar waitingForInput False
    if not needsInput then do 
        threadDelay (100)
        if waitingTime > 100 then do 
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

startUp:: [Int] -> DeviceContext -> MVar Int -> MVar [Int] -> MVar Bool -> IO()
startUp program context programIn programOut waitingForInput = do
    forkIO $ compute (program ++ [0,0..]) 0 0 programIn programOut waitingForInput
    threadDelay (2000 * 1000) 
    draw context programIn programOut 18 waitingForInput [[1]]

draw :: DeviceContext -> MVar Int -> MVar [Int] -> Int -> MVar Bool -> [[Int]] ->  IO ()
draw context programIn newBricks paddlePos waitingForInput mainBricks = do
    needsInput <- swapMVar waitingForInput False
    if not needsInput then do 
        threadDelay (1000000)
        putStrLn "Waiting"
        draw context programIn newBricks paddlePos waitingForInput mainBricks
    else do
        -- putStrLn "Read insts"
        -- tmpInst <- modifyMVar inst (\input -> return (input, take (4082 * 3) input))
        tmpNewBricks <- readMVar newBricks
        -- putMVar inst []
        
        let newBricksChunks = chunksOf 3 tmpNewBricks
            updatedMainBricks = deleteBrokenBricks mainBricks newBricksChunks
            w = width context
            h = height context
        putStrLn $ show newBricksChunks
        -- putStrLn $ show instructions
        -- putStrLn "Put insts"
        -- putMVar inst $ concat $ reverse instructions
        putStrLn "Sending"
        send context $ do 
            clearCanvas
            sequence_ [buildCanvas x y kind w h | (kind:y:x:_) <- updatedMainBricks]
            sequence_ [buildCanvas x y kind w h | (kind:y:x:_) <- newBricksChunks]
            stroke()

        putStrLn "Send"

        let (paddleChange, ballPos, score) = playTurn tmpNewBricks paddlePos (0,0,0)
            newPaddlePos = paddlePos + paddleChange
        -- putStrLn "after"
        putStrLn $ "paddleChange: " ++ show paddleChange
        putStrLn $ "paddle: " ++ show newPaddlePos
        putStrLn $ "ball: " ++ show ballPos
        putStrLn $ "score: " ++ show score


        -- putStrLn "about to set pin"
        putMVar programIn paddleChange
        -- putStrLn "Waiting"
        -- event <- wait context
        -- print event
        
        -- putMVar programIn $ case (eType event, eWhich event) of
        --     ("keydown", Just 37) -> -1
        --     ("keydown", Just 39) -> 1
        --     ("keyup", _) -> 0
        --     ("keydown", _) -> 0
        -- threadDelay (1000 * 1000) 
        draw context programIn newBricks newPaddlePos waitingForInput updatedMainBricks

deleteBrokenBricks :: [[Int]] -> [[Int]] -> [[Int]]
deleteBrokenBricks mainBricks [] = mainBricks
deleteBrokenBricks [] _ = []
deleteBrokenBricks mainBricks ([k, y, x]:newBricks) = deleteBrokenBricks (map (\[mK, mY, mX] -> 
     if k == 0 && mY == y && mX == x then [k, y, x] else [mK, mY, mX]) mainBricks) newBricks
        
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
    


buildCanvas :: Int -> Int -> Int -> Int -> Int -> Canvas ()
buildCanvas xIn yIn kind w h
    | xIn == (-1) && yIn == 0 = do
        beginPath()
        fillStyle "purple"
        font "48px serif"
        fillText(T.pack ("Score: " ++ show kind), fromIntegral (w `div` 2 - 100), 
            fromIntegral (h `div` 2 + 200))
        closePath()
    | otherwise = 
        case kind of 
            0 -> do 
                beginPath()
                fillStyle "yellow"
                rect (x, y, wBox, hBox)
                closePath()
                fill()
            1 -> do
                    beginPath()
                    fillStyle "brown"
                    rect (x, y, wBox, hBox)
                    closePath()
                    fill()
            2 -> do
                    beginPath()
                    fillStyle "blue"
                    rect (x, y, wBox, hBox)
                    closePath()
                    fill()
            3 -> do
                    beginPath()
                    fillStyle "orange"
                    rect (x, y, wBox, hBox)
                    closePath()
                    fill()
            4 -> do
                    beginPath()
                    fillStyle "red"
                    arc (x + r, y + r, r, 0, 2*pi, False)
                    closePath()
                    fill()
    where
        x = fromIntegral $ 50 + (w `div` 40) * xIn
        y = fromIntegral $ 50 + (h `div` 40) * yIn
        wBox = 20
        hBox = 20
        r = 10

putNewBlocks :: [[Int]] -> Int -> [[Int]]
putNewBlocks blocks 0 = blocks
putNewBlocks blocks 1082 = blocks
putNewBlocks (block:blocks) num = 
    putNewBlocks (map (\(kCur:yCur:xCur:_) -> if xCur == x && yCur == y 
        then [kind, y, x] else [kCur, yCur, xCur]) blocks) (num - 1)
    where 
        (kind:y:x:_) = block

compute :: [Int] -> Int -> Int -> MVar Int -> MVar [Int] -> MVar Bool -> IO ()
compute program index relBIndex input output waitingForInput
    | opcode == 99 = do 
        -- putMVar output [(-1)]
        putStrLn "Done computing"
    | opcode == 1 = compute (replaceNWithMode program (instructions !! 3) 
                        add relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 2 = compute (replaceNWithMode program (instructions !! 3) 
                        multiply relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 3 = do
        waiting <- isEmptyMVar input
        swapMVar waitingForInput ( if waiting then True else False)
        inputFromMVar <- takeMVar input
        compute (replaceNWithMode program (instructions !! 1) inputFromMVar 
            relBIndex paraOneMode) endIndex relBIndex input output waitingForInput
    | opcode == 4 = do
        -- emptyOut <- isEmptyMVar output
        -- if emptyOut then do putMVar output [(takeWithMode program (instructions !! 1) relBIndex paraOneMode)] else do
        modifyMVar_ output (\ oldVal -> return ((takeWithMode program (instructions !! 1) relBIndex paraOneMode):oldVal))
        compute program endIndex relBIndex input output waitingForInput
    | opcode == 5 = if (jumpVal /= 0) 
                        then compute program jumpToIndex relBIndex input output waitingForInput
                        else compute program endIndex relBIndex input output waitingForInput
    | opcode == 6 = if (jumpVal  == 0) 
                        then compute program jumpToIndex  relBIndex input output waitingForInput
                        else compute program endIndex relBIndex input output waitingForInput
    | opcode == 7 = compute (replaceNWithMode program (instructions !!3) lessThan 
                        relBIndex paraThreeMode) endIndex relBIndex input output waitingForInput
    | opcode == 8 = compute (replaceNWithMode program (instructions !!3) equals relBIndex paraThreeMode) 
                        endIndex relBIndex input output waitingForInput
    | opcode == 9 = compute program endIndex (relBIndex + 
                        (takeWithMode program (instructions !! 1) relBIndex paraOneMode)) input output waitingForInput
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

replaceN :: [a] -> Int -> a -> [a]
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
