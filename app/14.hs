import System.IO
import qualified  Data.Map as Map
import Data.List.Split
import Data.Tree
import Data.List
import Data.Maybe

main :: IO ()
main = do 
    handle <- openFile "data/input14.txt" ReadMode
    inputMap <- makeMap handle Map.empty
    let keyMap = makeKeyMap inputMap
        tree = unfoldTree (\key -> (key, makeChildNodes key inputMap)) (1, "FUEL")
        deltas = Map.toList $ foldTree (\node nodeResults -> makeDeltaMap node nodeResults inputMap) tree
        totalOreTree = totalOfKeyInTree tree "ORE"
        totalOre = calcTotal deltas keyMap inputMap totalOreTree 0
    putStrLn "The equations in a tree that are to be solved: \r\n"
    putStrLn $ drawTree $ fmap show $ tree
    putStrLn $ "The minimum number of ORE required is: " ++ show totalOre
    putStrLn $ show deltas
    putStrLn $ show totalOreTree
    putStrLn $ show totalOre

calcTotal :: [(String, Int)] -> Map.Map String (Int, String) -> 
             Map.Map (Int, String) [(Int, String)] -> Int -> Int -> Int
calcTotal [] _ _ totalOreTree totalSaved = totalOreTree - totalSaved
calcTotal (delta:deltas) keyMap inputMap totalOreTree totalSaved = 
    calcTotal deltas keyMap inputMap totalOreTree (totalSaved + 
        calcNumKeySaved delta keyMap inputMap "ORE")

calcNumKeySaved :: (String, Int) -> Map.Map String (Int, String) -> 
             Map.Map (Int, String) [(Int, String)] -> String -> Int
calcNumKeySaved delta keyMap inputMap key = do
    let timesDelta = (snd delta) `div` (fst $ keyMap Map.! (fst delta))
    if timesDelta < 1 then 0
    else do
        let results = inputMap Map.! (keyMap Map.! (fst delta))
            maybeKey = find (\(num, name) -> name == key) results 
        if isJust maybeKey then (snd delta) - (snd delta `mod` (fst $ fromJust maybeKey)) -- !
        else do 
            sum $ map (\(num, name) -> calcNumKeySaved (name, num * timesDelta) keyMap inputMap key) results 

makeDeltaMap :: (Int, String) -> [Map.Map String Int] -> Map.Map (Int, String) [(Int, String)] -> 
    Map.Map String Int
makeDeltaMap node nodeResults inputMap = do 
    if Map.member node inputMap then Map.unionsWith (+) (nodeResults)
    else do 
        let maybeKey = find (\(num, name) -> name == snd node) $ Map.keys inputMap
            nodeVal = fst node
            actualVal = fst $ fromJust maybeKey
        if isNothing maybeKey then Map.unionsWith (+) (nodeResults)
            else Map.unionsWith (+) 
                ((Map.singleton (snd node) (if floor (fromIntegral nodeVal / fromIntegral actualVal) == 
                    ceiling (fromIntegral nodeVal / fromIntegral actualVal) then 0 
                    else actualVal `mod` nodeVal)):nodeResults)

totalOfKeyInTree :: Tree (Int, String) -> String -> Int
totalOfKeyInTree tree kind = foldTree (\(num, name) nodeResults -> 
    if name == kind then sum (num:nodeResults) else sum nodeResults) tree

makeChildNodes :: (Int, String) -> Map.Map (Int, String) [(Int, String)] -> [(Int, String)]
makeChildNodes key inputMap = do
    if Map.member key inputMap then inputMap Map.! key 
    else do
        let maybeKey = find (\(num, name) -> name == snd key) $ Map.keys inputMap
        if isNothing maybeKey then []
        else map (\(num, name) -> (num * (ceiling (fromIntegral (fst key) / fromIntegral (fst $ fromJust maybeKey))), name))
            $ inputMap Map.! (fromJust maybeKey)

makeMap :: Handle -> Map.Map (Int, String) [(Int, String)] -> IO (Map.Map (Int, String) [(Int, String)])
makeMap handle accMap = do
    isEOF <- hIsEOF handle
    if isEOF then return accMap 
    else do
        line <- hGetLine handle
        let lineTuples = map tuplify2 $ map words $ concat $ map (\ingRes -> splitOn "," ingRes) $ splitOn "=>" line
            entryIngredients = init lineTuples
            entryResult =  last lineTuples
        makeMap handle $ Map.insert entryResult entryIngredients accMap

tuplify2 :: [String] -> (Int, String)
tuplify2 [x,y] = (read x :: Int, y)

makeKeyMap :: Map.Map (Int, String) [(Int, String)] -> Map.Map String (Int, String)
makeKeyMap inputMap = Map.fromList $ map (\(a, b) -> (b, (a, b))) $ Map.keys inputMap
