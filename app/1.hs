import System.IO
import Safe 

main :: IO ()
main = do
    handle <- openFile "data/input.txt" ReadMode
    calcFuel handle 0
    hClose handle

calcFuel :: Handle -> Int -> IO ()
calcFuel handle fuelSoFar = do
    eof <- hIsEOF handle
    if (not eof) then do
        maybeInt <- readLine handle
        case maybeInt of
            (Just moduleMass) -> calcFuel handle 
                (fuelSoFar + moduleMass `div` 3 -2)
            Nothing -> putStrLn "What was read was not an integer."
    else 
        putStrLn $ "Fuel used: " ++ show fuelSoFar

readLine :: Handle -> IO (Maybe Int)
readLine handle = do 
    line <- hGetLine handle
    return (readMay line)