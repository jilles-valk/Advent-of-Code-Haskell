import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do 
    handle <- openFile "data/input11.txt" ReadMode
    contents <- hGetContents handle
    let program = map read $ splitOn "," contents :: [Int]
    paint program

paint :: [Int] -> IO ()
paint program = do
    computerInput <- newMVar 0
    painting <- newMVar []
    robotInstruction <- newEmptyMVar
    c <-  async $ compute (program ++ [0,0..]) 0 0 computerInput robotInstruction
    d <- async $ driveRobot painting robotInstruction computerInput (0, 0) 1
    putStrLn $ "Started threads."
    waitBoth c d
    putStrLn "Finished threads."
    printPainting painting
    putStrLn "Finished."

printPainting :: MVar [((Int, Int), Int)] -> IO [()]
printPainting painting = do
    toPaint <- readMVar painting
    let minX = fst $ fst $ minimumBy (\((xa, _), _) ((xb, _), _) -> compare xa xb) toPaint
        minY = snd $ fst $ minimumBy (\((_, ya), _) ((_, yb), _) -> compare ya yb) toPaint
        -- maxX = fst $ fst $ maximumBy (\((xa, _), _) ((xb, _), _) -> compare xa xb) toPaint
        -- maxY = snd $ fst $ maximumBy (\((_, ya), _) ((_, yb), _) -> compare ya yb) toPaint
        -- width = abs minX + abs maxX
        -- height = abs minY + abs maxY
        sortedPainting = map (sortBy (\((xa, _), _) ((xb, _), _) -> compare xa xb)) 
            $ groupBy (\((_, ya), _) ((_, yb), _) -> ya == yb) $ sortBy 
            (\((_, ya), _) ((_, yb), _) -> compare ya yb) toPaint
    traverse (putStrLn ) $ map (\row -> concat $ snd $ mapAccumL (\x ((xPos, yPos), colour) -> 
        if x == xPos then 
            if colour == 1 then (x + 1, "#")
            else (x + 1, " ")
        else (xPos + 1, replicate (abs (xPos - x)) ' ')) minX row) sortedPainting   

driveRobot :: MVar [((Int, Int), Int)] -> MVar Int -> MVar Int -> (Int, Int) -> Int -> IO ()
driveRobot painting instruction colour position direction =  do
    drivingInstruction <- takeMVar instruction
    if drivingInstruction == -1 then do putStrLn "Done driving" else do
        colourToPaint <- takeMVar instruction
        tempPainting <- takeMVar painting
        let tempTempPainting = filter (\(pos, _) -> 
                if pos /= position then True else False) tempPainting
            tempTempTempPainting = insert (position, colourToPaint) tempTempPainting
        putMVar painting tempTempTempPainting
        let newDirection = if drivingInstruction == 0 then (direction + 3) `mod` 4 
            else (direction - 3) `mod` 4
            newPos = case newDirection of
                0 -> (fst position - 1, snd position)
                1 -> (fst position, snd position + 1)
                2 -> (fst position + 1, snd position)
                3 -> (fst position, snd position - 1)
            newColour = find (\elem -> if (fst elem) == newPos then True 
                else False) tempTempTempPainting
        if isJust newColour then putMVar colour $ snd $ fromJust newColour 
        else putMVar colour 0 
        driveRobot painting instruction colour newPos newDirection

compute :: [Int] -> Int -> Int -> MVar Int -> MVar Int -> IO ()
compute program index relBIndex input output
    | opcode == 99 = do 
        putMVar output (-1)
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
        putMVar output (takeWithMode program (instructions !! 1) relBIndex paraOneMode)
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