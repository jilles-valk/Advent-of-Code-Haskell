import System.IO
import Data.List.Split
import Data.Tree
import Data.Maybe

main :: IO ()
main = do
    handle <- openFile "data/input6.txt" ReadMode
    contents <- hGetContents handle
    -- input needs empty line at end due to using \r\n
    let treeInput = map tuplify2 $ map (splitOn ")") $ splitOn "\r\n" contents
        tree = unfoldTree (\name -> (name, findChildren treeInput [] name)) "COM"
        treeLevels = levels tree
        numOrbits = countOrbits treeLevels 0 0
        youParent = findParent treeInput "YOU"
        sanParent = findParent treeInput "SAN"
        shortestDistanceYouSanParent = 
            shortestDistance tree youParent sanParent (maxBound :: Int)
    putStrLn $ "Total number of orbits: " ++ show numOrbits
    putStrLn $ "Shortest distance between you and Santa: " ++ 
        show shortestDistanceYouSanParent

findChildren :: [(String, String)] -> [String]-> String -> [String]
findChildren (node:otherNodes) foundChildren name
    | null otherNodes = foundChildren
    | fst node == name = findChildren otherNodes ((snd node):foundChildren) name 
    | otherwise = findChildren otherNodes foundChildren name

findParent :: [(String, String)] -> String -> String
findParent (node:otherNodes) name
    | null otherNodes = if snd node == name then fst node else ""
    | snd node == name = fst node
    | otherwise = findParent otherNodes name

countOrbits :: [[a]] -> Int -> Int -> Int
countOrbits (level:levels) depth count
    | null levels = newCount
    | otherwise = countOrbits levels (depth + 1) newCount
    where 
        newCount = count + (length level)*depth

shortestDistance :: Tree String -> String -> String -> Int -> Int
shortestDistance tree youParent sanParent shortest 
    | isJust maybeTreeDist && distance < shortest = 
        shortestDistance subTree youParent sanParent distance
    | otherwise = shortest
    where
        maybeTreeDist = subTreeWith (subForest tree) youParent sanParent
        (subTree, distYouParent, distSanParent) = fromJust maybeTreeDist
        distance = distYouParent + distSanParent

subTreeWith :: [Tree String] -> String -> String -> Maybe (Tree String, Int, Int)
subTreeWith (tree:forest) node1 node2
    | treeHasChild = 
        Just (tree, fromJust distNode1, fromJust distNode2)
    | not treeHasChild && not (null forest) = subTreeWith forest node1 node2
    | otherwise = Nothing
    where
        distNode1 = distToChild [tree] node1 0
        distNode2 = distToChild [tree] node2 0
        treeHasChild = isJust distNode1 && isJust distNode2

distToChild :: [Tree String] -> String -> Int -> Maybe Int
distToChild [] _ distance = Nothing
distToChild (tree:forest) child distance
    | rootLabel tree == child = Just distance
    | isJust childInTreeSubforestDist = childInTreeSubforestDist
    | null forest && null (subForest tree) = Nothing
    | otherwise = distToChild forest child (distance)
    where
        childInTreeSubforestDist = distToChild (subForest tree) child (distance + 1) 

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)