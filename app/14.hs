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
        tree = unfoldTree (\key -> (key, makeChildNodes key keyMap inputMap)) (460665, "FUEL")
        totalOre = calcTotal tree keyMap inputMap "ORE"
    putStrLn $ "The minimum number of ORE required is: " ++ show totalOre
    let fuelForOneTrillion = maxFuelForSomeOre keyMap inputMap (0, 0) 1000000000000
    putStrLn $ "The maximum amount of fuel for 1000000000000 units of ore is: " ++ show fuelForOneTrillion

maxFuelForSomeOre :: Map.Map String (Int, String) -> Map.Map (Int, String) [(Int, String)] -> 
                    (Int, Int) -> Int -> Int
maxFuelForSomeOre keyMap inputMap (fuel, ore) someOre = do
    let treeOne = unfoldTree (\key -> (key, makeChildNodes key keyMap inputMap)) (fuel - 1, "FUEL")
        treeTwo = unfoldTree (\key -> (key, makeChildNodes key keyMap inputMap)) (fuel + 1, "FUEL")
        derivOre = (calcTotal treeTwo keyMap inputMap "ORE") - (calcTotal treeOne keyMap inputMap "ORE")
        newFuelAmount = round $ (fromIntegral fuel) - (fromIntegral (ore - someOre) / fromIntegral derivOre)
        tree = unfoldTree (\key -> (key, makeChildNodes key keyMap inputMap)) (newFuelAmount, "FUEL")
        newOreAmount = calcTotal tree keyMap inputMap "ORE"
    if abs (newFuelAmount - fuel) < 1 then newFuelAmount 
    else maxFuelForSomeOre keyMap inputMap (newFuelAmount, newOreAmount) someOre

calcTotal :: Tree (Int, String) -> Map.Map String (Int, String) -> Map.Map (Int, String) [(Int, String)] -> 
             String -> Int
calcTotal tree keyMap inputMap key = do
    let deltas = foldTree (\node nodeResults -> makeDeltaMap node nodeResults keyMap inputMap) tree
        reducedDeltas = reduceAll deltas Map.empty keyMap inputMap
        totalOreTree = totalOfKeyInTree tree key
    totalOreTree - (reducedDeltas Map.! key)

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
