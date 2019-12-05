import System.IO
import Data.List.Split

main :: IO ()
main = do 
    handle <- openFile "data/input2.txt" ReadMode
    contents <- hGetContents handle
    let input = map read $ splitOn "," contents :: [Int]
    putStrLn $ "The output for first value 19690720 is: " 
        ++ show (computeNounVerb input 0 0 19690720)

computeNounVerb :: [Int] -> Int -> Int -> Int -> Int
computeNounVerb input noun verb requiredAtZero
    | noun == 100 && verb == 100 = -1
    | result == requiredAtZero = 100*noun + verb
    | verb == 100 = computeNounVerb input (noun + 1) 0 requiredAtZero
    | otherwise = computeNounVerb input noun (verb + 1) requiredAtZero
    where 
        result = compute (replaceN (replaceN input 1 noun) 2 verb) 0

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

