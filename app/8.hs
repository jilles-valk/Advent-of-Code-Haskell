import System.IO
import Data.List.Split
import Data.Foldable

main :: IO ()
main = do 
    handle <- openFile "data/input8.txt" ReadMode
    contents <- hGetContents handle
    let imgHeight = 6
        imgWidth = 25
        imgLayers = map (chunksOf imgWidth) $ chunksOf (imgWidth*imgHeight) contents 
        fzLayer = fewestZeroLayers imgLayers [] (maxBound :: Int)
        result = resultLayer fzLayer
        image = makeImage imgLayers
    putStrLn $ "The number of 1's times the number of 2's in the layer with the lowest number of 0's is: " 
        ++ show result
    putStrLn "The image:"
    printImage image

printImage :: [[Char]] -> IO ()
printImage img = traverse_ (\arr -> putStrLn 
    (map (\pixel -> if pixel == '1' then '#' else ' ') arr)) img

makeImage :: [[[Char]]] -> [[Char]]
makeImage layers = foldl addLayer [[]] layers

addLayer :: [[Char]] -> [[Char]] -> [[Char]]
addLayer [[]] layer = layer
addLayer oldImage layer = zipWith addRow oldImage layer

addRow :: [Char] -> [Char] -> [Char]
addRow rowOne rowTwo = zipWith addPixel rowOne rowTwo

addPixel :: Char -> Char -> Char
addPixel pixelB pixelA
    | pixelB == '2' = pixelA
    | otherwise = pixelB

fewestZeroLayers :: [[String]] -> [[Char]] -> Int -> [[Char]]
fewestZeroLayers (thisLayer:otherLayers) layer numZero
    | null otherLayers = newLayer
    | otherwise = fewestZeroLayers otherLayers newLayer newNumZero
    where
        thisNumZero = foldl (\count x -> count + countNumberAInList x '0') 0 thisLayer
        thisLessZero = thisNumZero /= 0 && thisNumZero < numZero
        newNumZero = if thisLessZero then thisNumZero else numZero
        newLayer = if thisLessZero then thisLayer else layer

countNumberAInList :: [Char] -> Char -> Int
countNumberAInList list a = 
    foldl (\count x -> if x == a then count + 1 else count) 0 list

resultLayer :: [[Char]] -> Int
resultLayer layer = 
    (foldl (\count x -> count + countNumberAInList x '1') 0 layer) * 
    (foldl (\count x -> count + countNumberAInList x '2') 0 layer)