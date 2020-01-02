import qualified Data.List as D
import qualified Data.Sequence as Seq

data Moon = Moon {
        xPos :: Int,
        yPos :: Int,
        zPos :: Int,
        xVel :: Int,
        yVel :: Int,
        zVel :: Int
    } deriving (Eq, Show)

main :: IO ()
main = do
    let moons = Seq.fromList [(Moon 17 (-9) 4 0 0 0), (Moon 2 2 (-13) 0 0 0),
                          (Moon (-1) 5 (-1) 0 0 0), (Moon 4 7 (-7) 0 0 0)]
        combis = Seq.fromList $ pairs $ [0 .. ((length moons) - 1)]
        simResMoons = runSim moons combis 1000
        (itForEqX, _) = findMoonSim moons moons combis 0 'x'
        (itForEqY, _) = findMoonSim moons moons combis 0 'y'
        (itForEqZ, _) = findMoonSim moons moons combis 0 'z'
        itEq = lcm itForEqZ $ lcm itForEqX itForEqY
    putStrLn $ "The final total energy is: " ++ show (totalEngergy simResMoons 0)
    putStrLn $ "Number of iterations to get back to initial state: " ++ show itEq
    
findMoonSim :: Seq.Seq (Moon) -> Seq.Seq (Moon) -> Seq.Seq ((Int, Int)) -> Int -> Char -> (Int, Seq.Seq (Moon))
findMoonSim initialMoons moons combis iteration axis
    | initialMoons == moons && iteration /= 0 = (iteration, moons)
    | otherwise = findMoonSim initialMoons newMoons combis (iteration + 1) axis
    where 
        moonsUpdateVel = updateVel moons combis axis
        newMoons = updatePos moonsUpdateVel moonsUpdateVel 0 axis

runSim :: Seq.Seq (Moon) -> Seq.Seq ((Int, Int)) -> Int -> Seq.Seq (Moon)
runSim moons combis stepsLeft
    | stepsLeft == 0 = moons
    | Seq.null moons = Seq.empty
    | otherwise = runSim newMoons combis (stepsLeft - 1)
    where 
        moonsUpdateVel = updateVel moons combis 'a'
        newMoons = updatePos moonsUpdateVel moonsUpdateVel 0 'a'

updateVel :: Seq.Seq (Moon) -> Seq.Seq ((Int, Int)) -> Char -> Seq.Seq (Moon)
updateVel moons combis axis
    | Seq.null combis = moons
    | otherwise = do
        let (a, b) = Seq.index combis 0
            moonA = Seq.index moons a
            moonB = Seq.index moons b
            newMoonA = case axis of
                'a' -> Moon (xPos moonA) (yPos moonA) (zPos moonA) 
                            (updateComponent (xPos moonA) (xPos moonB) (xVel moonA))
                            (updateComponent (yPos moonA) (yPos moonB) (yVel moonA))
                            (updateComponent (zPos moonA) (zPos moonB) (zVel moonA))
                'x' -> Moon (xPos moonA) (yPos moonA) (zPos moonA) 
                            (updateComponent (xPos moonA) (xPos moonB) (xVel moonA))
                            (yVel moonA) (zVel moonA)
                'y' -> Moon (xPos moonA) (yPos moonA) (zPos moonA) (xVel moonA)
                            (updateComponent (yPos moonA) (yPos moonB) (yVel moonA)) (zVel moonA)
                'z' -> Moon (xPos moonA) (yPos moonA) (zPos moonA) (yVel moonA) (xVel moonA)
                            (updateComponent (zPos moonA) (zPos moonB) (zVel moonA))
            newMoonB = case axis of
                'a' -> Moon (xPos moonB) (yPos moonB) (zPos moonB) 
                            (updateComponent (xPos moonB) (xPos newMoonA) (xVel moonB))
                            (updateComponent (yPos moonB) (yPos newMoonA) (yVel moonB))
                            (updateComponent (zPos moonB) (zPos newMoonA) (zVel moonB))
                'x' -> Moon (xPos moonB) (yPos moonB) (zPos moonB) 
                            (updateComponent (xPos moonB) (xPos moonA) (xVel moonB))
                            (yVel moonB) (zVel moonB)
                'y' -> Moon (xPos moonB) (yPos moonB) (zPos moonB) (xVel moonB)
                            (updateComponent (yPos moonB) (yPos moonA) (yVel moonB)) (zVel moonB)
                'z' -> Moon (xPos moonB) (yPos moonB) (zPos moonB) (yVel moonB) (xVel moonB)
                            (updateComponent (zPos moonB) (zPos moonA) (zVel moonB))
        updateVel (Seq.update b newMoonB (Seq.update a newMoonA moons)) (Seq.drop 1 combis) axis

updateComponent :: Int -> Int -> Int -> Int
updateComponent a b val = case compare a b of
                            LT -> val + 1 
                            GT -> val - 1
                            EQ -> val

updatePos :: Seq.Seq (Moon) -> Seq.Seq (Moon) -> Int -> Char -> Seq.Seq (Moon)
updatePos oldMoons newMoons index axis
    | Seq.null oldMoons = newMoons
    | otherwise = updatePos (Seq.drop 1 oldMoons) (Seq.update index newMoon newMoons) (index + 1) axis
    where 
        oldMoon = Seq.index oldMoons 0
        newMoon = case axis of 
            'a' -> Moon (xPos oldMoon + xVel oldMoon)
                       (yPos oldMoon + yVel oldMoon)
                       (zPos oldMoon + zVel oldMoon)
                       (xVel oldMoon) (yVel oldMoon) (zVel oldMoon)
            'x' -> Moon (xPos oldMoon + xVel oldMoon) (yPos oldMoon) (zPos oldMoon) 
                        (xVel oldMoon) (yVel oldMoon) (zVel oldMoon)
            'y' -> Moon (xPos oldMoon) (yPos oldMoon + yVel oldMoon)
                       (zPos oldMoon) (xVel oldMoon) (yVel oldMoon) (zVel oldMoon)
            'z' -> Moon (xPos oldMoon) (yPos oldMoon) (zPos oldMoon + zVel oldMoon)
                        (xVel oldMoon) (yVel oldMoon) (zVel oldMoon)

pairs :: [a] -> [(a, a)]
pairs xs =[ (x,y) | (x:rest) <- D.tails xs , y <- rest]

totalEngergy :: Seq.Seq (Moon) -> Int -> Int
totalEngergy moons energySoFar
    | Seq.null moons = energySoFar
    | otherwise = totalEngergy (Seq.drop 1 moons) newEnergy
    where 
        moon = Seq.index moons 0
        ePot = abs (xPos moon) + abs (yPos moon) + abs (zPos moon)
        eKin = abs (xVel moon) + abs (yVel moon) + abs (zVel moon)
        newEnergy = energySoFar + ePot * eKin
