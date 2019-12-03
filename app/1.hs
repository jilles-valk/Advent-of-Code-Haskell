import System.IO
import Safe 

main :: IO ()
main = do
    handle <- openFile "data/input1.txt" ReadMode
    calcFuel handle 0
    hClose handle

calcFuel :: Handle -> Int -> IO ()
calcFuel handle fuelSoFar = do
    eof <- hIsEOF handle
    if (not eof) then do
        maybeInt <- readLine handle
        case maybeInt of
            (Just moduleMass) -> calcFuel handle $ fuelSoFar + calcFuelThisModule moduleMass
            Nothing -> putStrLn "What was read was not an integer."
    else 
        putStrLn $ "Fuel used: " ++ show fuelSoFar

readLine :: Handle -> IO (Maybe Int)
readLine handle = do 
    line <- hGetLine handle
    return (readMay line)

calcFuelThisModule :: Int -> Int
calcFuelThisModule moduleMass = 
    let fuelForMass = calcFuelForMass moduleMass
        fuelForFuel = calcFuelForFuel 0 fuelForMass
    in fuelForMass + fuelForFuel

calcFuelForFuel :: Int -> Int -> Int
calcFuelForFuel fuelSoFar fuelMass 
    | fuelMass <= 0 = fuelSoFar
    | otherwise = calcFuelForFuel (fuelSoFar + fuelForMass) fuelForMass 
    where 
        fuelForMass = calcFuelForMass fuelMass

calcFuelForMass :: Int -> Int
calcFuelForMass moduleMass 
    | fuel < 0 = 0
    | otherwise = fuel
    where 
        fuel = moduleMass `div` 3 - 2