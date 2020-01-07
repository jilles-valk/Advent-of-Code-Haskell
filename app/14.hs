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
    putStrLn $ show inputMap
    let tree = unfoldTree (\key -> (key, makeChildNodes key inputMap)) (1, "FUEL")
    putStr $ drawTree $ fmap show $ tree

makeChildNodes :: (Double, String) -> Map.Map (Double, String) [(Double, String)] -> [(Double, String)]
makeChildNodes key inputMap = do
    if Map.member key inputMap then inputMap Map.! key 
    else do
        let maybeKey = find (\(num, name) -> name == snd key) $ Map.keys inputMap
        if isNothing maybeKey then []
        else map (\(num, name) -> (num * ((fst key) / (fst $ fromJust maybeKey)), name))
            $ inputMap Map.! (fromJust maybeKey)
        -- else inputMap Map.! (fromJust maybeKey)

makeMap :: Handle -> Map.Map (Double, String) [(Double, String)] -> IO (Map.Map (Double, String) [(Double, String)])
makeMap handle accMap = do
    isEOF <- hIsEOF handle
    if isEOF then return accMap 
    else do
        line <- hGetLine handle
        let lineTuples = map tuplify2 $ map words $ concat $ map (\ingRes -> splitOn "," ingRes) $ splitOn "=>" line
            entryIngredients = init lineTuples
            entryResult =  last lineTuples
        makeMap handle $ Map.insert entryResult entryIngredients accMap

tuplify2 :: [String] -> (Double, String)
tuplify2 [x,y] = (read x :: Double, y)