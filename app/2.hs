import System.IO
import Data.List.Split

main :: IO ()
main = do 
    handle <- openFile "data/input2.txt" ReadMode
    contents <- hGetContents handle
    let temp = map read $ splitOn "," contents :: [Int]
        input = replaceN (replaceN temp 1 12) 2 2
    putStrLn $ "The first value is: " ++ show (compute input 0)


compute :: [Int] -> Int -> Int
compute input index
    | length input == index = head input
    | opcode == 99 = head input
    | opcode == 1 = compute (replaceN input (instructions !! 3) add) endIndex
    | opcode == 2 = compute (replaceN input (instructions !! 3) multiply) endIndex
    | otherwise = compute input (index + 4)
    where 
        endIndex = index + 4
        instructions = drop index . take endIndex $ input
        opcode = head instructions
        add = input !! (instructions !! 1) + input !! (instructions !! 2)
        multiply = input !! (instructions !! 1) * input !! (instructions !! 2)
        
replaceN :: [Int] -> Int -> Int -> [Int]
replaceN (x:xs) n newVal
    | n == 0 = newVal:xs
    | otherwise = x: replaceN xs (n-1) newVal

