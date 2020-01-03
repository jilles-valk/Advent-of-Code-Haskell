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

    forkIO $ compute (program ++ [0,0..]) 0 0 programIn programOut
    threadDelay (1000 * 1000) 
    instructions <- readMVar programOut
    -- let instructions = reverse [-1, 0, 1]
    putStrLn $ "The number of blocks is: " ++ show (countTypeObject instructions 2 0)
    blankCanvas 3000  { events = ["keyup","keydown"] } $ \ context -> draw context programIn programOut

countTypeObject :: [Int] -> Int -> Int -> Int
countTypeObject [] _ count = count
countTypeObject (kind:_:_:instructions) isKind count = if isKind == kind 
    then countTypeObject instructions isKind (count + 1) 
    else countTypeObject instructions isKind count 

draw :: DeviceContext -> MVar Int -> MVar [Int] ->  IO ()
draw context programIn inst = do
    putStrLn "Read insts"
    tmpInst <- readMVar inst
    let instructions = putNewBlocks (chunksOf 3 tmpInst) 30
        w = width context
        h = height context
    putStrLn "Put insts"
    -- putMVar inst (concat instructions)
    putStrLn "Sending"
    send context $ do 
        clearCanvas
        sequence_ [buildCanvas x y kind w h | (kind:y:x:_) <- instructions]
        stroke()
        
    putStrLn "Waiting"
    event <- wait context
    print event
    threadDelay (100 * 1000) 
    putMVar programIn $ case (eType event, eWhich event) of
        ("keydown", Just 37) -> -1
        ("keydown", Just 39) -> 1
        ("keyup", _) -> 0
        ("keydown", _) -> 0
    putStrLn "Set mvar"
    threadDelay (100 * 1000) 
    draw context programIn inst


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
putNewBlocks (block:blocks) num = 
    putNewBlocks (map (\(kCur:yCur:xCur:_) -> if xCur == x && yCur == y 
        then [kind, y, x] else [kCur, yCur, xCur]) blocks) (num - 1)
    where 
        (kind:y:x:_) = block

compute :: [Int] -> Int -> Int -> MVar Int -> MVar [Int] -> IO ()
compute program index relBIndex input output
    | opcode == 99 = do 
        -- putMVar output [(-1)]
        putStrLn "Done computing"
    | opcode == 1 = compute (replaceNWithMode program (instructions !! 3) 
                        add relBIndex paraThreeMode) endIndex relBIndex input output
    | opcode == 2 = compute (replaceNWithMode program (instructions !! 3) 
                        multiply relBIndex paraThreeMode) endIndex relBIndex input output
    | opcode == 3 = do
        inputFromMVar <- takeMVar input
        compute (replaceNWithMode program (instructions !! 1) inputFromMVar 
            relBIndex paraOneMode) endIndex relBIndex input output
    | opcode == 4 = do
        -- emptyOut <- isEmptyMVar output
        -- if emptyOut then do putMVar output [(takeWithMode program (instructions !! 1) relBIndex paraOneMode)] else do
        modifyMVar_ output (\ oldVal -> return ((takeWithMode program (instructions !! 1) relBIndex paraOneMode):oldVal))
        compute program endIndex relBIndex input output
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
