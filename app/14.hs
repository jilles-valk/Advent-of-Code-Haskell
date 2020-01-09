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
        tree = unfoldTree (\key -> (key, makeChildNodes key keyMap inputMap)) (1, "FUEL")
        deltas = foldTree (\node nodeResults -> makeDeltaMap node nodeResults keyMap inputMap) tree
        reducedDeltas = reduceAll deltas Map.empty keyMap inputMap
        totalOreTree = totalOfKeyInTree tree "ORE"
        totalOre = totalOreTree - (reducedDeltas Map.! "ORE")
    putStrLn $ "The minimum number of ORE required is: " ++ show totalOre

reduceAll :: Map.Map String Int -> Map.Map String Int -> Map.Map String (Int, String) -> 
             Map.Map (Int, String) [(Int, String)] -> Map.Map String Int
reduceAll thisDeltas previousDeltas keyMap inputMap 
    | thisDeltas == previousDeltas = thisDeltas
    | otherwise = reduceAll (Map.foldlWithKey (\accMap key val -> Map.unionWith (+) 
        (reduceDelta (key, val) keyMap inputMap) accMap) Map.empty thisDeltas) thisDeltas keyMap inputMap

reduceDelta :: (String, Int) -> Map.Map String (Int, String) -> 
                Map.Map (Int, String) [(Int, String)] -> Map.Map String Int
reduceDelta delta keyMap inputMap = do
    if Map.notMember (fst delta) keyMap then Map.singleton (fst delta) (snd delta)
    else do
        let keyForDelta = keyMap Map.! fst delta
            timesDelta = (snd delta) `div` (fst keyForDelta)
        if timesDelta < 1 then Map.singleton (fst delta) (snd delta)
        else do 
            let results = inputMap Map.! keyForDelta
            if not $ null results then do
                let newDelta = (fst delta, (snd delta `mod` fst keyForDelta))
                Map.fromList (newDelta:(map (\(num, name) -> (name, timesDelta * num)) results))
            else Map.singleton (fst delta) (snd delta)

makeDeltaMap :: (Int, String) -> [Map.Map String Int] -> Map.Map String (Int, String) -> 
                Map.Map (Int, String) [(Int, String)] -> Map.Map String Int
makeDeltaMap node nodeResults keyMap inputMap = do
    if Map.notMember (snd node) keyMap then Map.empty
    else if Map.member node inputMap then Map.unionsWith (+) (nodeResults)
        else do 
            let key = keyMap Map.! (snd node)
                nodeVal = fst node
                actualVal = fst key
            Map.unionsWith (+) 
                ((Map.singleton (snd node) 
                ((ceiling (fromIntegral nodeVal / fromIntegral actualVal) * (fromIntegral actualVal)) - nodeVal))
                :nodeResults)

totalOfKeyInTree :: Tree (Int, String) -> String -> Int
totalOfKeyInTree tree kind = foldTree (\(num, name) nodeResults -> 
    if name == kind then sum (num:nodeResults) else sum nodeResults) tree

makeChildNodes :: (Int, String) -> Map.Map String (Int, String) -> Map.Map (Int, String) [(Int, String)] -> 
                [(Int, String)]
makeChildNodes key keyMap inputMap = do
    if Map.member key inputMap then inputMap Map.! key 
    else do
        let maybeKey = find (\(num, name) -> name == snd key) $ Map.keys inputMap
        if Map.notMember (snd key) keyMap then []
        else do 
            let actualKey = keyMap Map.! snd key
            map (\(num, name) -> (num * (ceiling (fromIntegral (fst key) / fromIntegral (fst $ actualKey))), name))
                $ inputMap Map.! actualKey

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
