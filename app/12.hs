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
    -- let moons = Seq.fromList [(Moon (-1) (0) (2) 0 0 0), (Moon (2) (-10) (-7) 0 0 0),
    --                       (Moon (4) (-8) (8) 0 0 0), (Moon (3) (5) (-1) 0 0 0)]
        combis = Seq.fromList $ pairs $ [0 .. ((length moons) - 1)]
        simResMoons = runSim moons combis 1000
   
    putStrLn $ "The final positions and velocities are: "
    putStrLn $ show simResMoons
    putStrLn $ "The final total energy is: " ++ show (totalEngergy simResMoons 0)
    

runSim :: Seq.Seq (Moon) -> Seq.Seq ((Int, Int)) -> Int -> Seq.Seq (Moon)
runSim moons combis stepsLeft
    | stepsLeft == 0 = moons
    | Seq.null moons = Seq.empty
    | otherwise = runSim (updatePos moonsUpdateVel moonsUpdateVel 0) combis (stepsLeft - 1)
    where 
        moonsUpdateVel = updateVel moons combis

updateVel :: Seq.Seq (Moon) -> Seq.Seq ((Int, Int)) -> Seq.Seq (Moon)
updateVel moons combis 
    | Seq.null combis = moons
    | otherwise = do
        let (a, b) = Seq.index combis 0
            moonA = Seq.index moons a
            moonB = Seq.index moons b
            newMoonA = Moon (xPos moonA) (yPos moonA) (zPos moonA) 
                            (updateComponent (xPos moonA) (xPos moonB) (xVel moonA))
                            (updateComponent (yPos moonA) (yPos moonB) (yVel moonA))
                            (updateComponent (zPos moonA) (zPos moonB) (zVel moonA))
            newMoonB = Moon (xPos moonB) (yPos moonB) (zPos moonB) 
                            (updateComponent (xPos moonB) (xPos newMoonA) (xVel moonB))
                            (updateComponent (yPos moonB) (yPos newMoonA) (yVel moonB))
                            (updateComponent (zPos moonB) (zPos newMoonA) (zVel moonB))
        updateVel (Seq.update b newMoonB (Seq.update a newMoonA moons)) (Seq.drop 1 combis)

updateComponent :: Int -> Int -> Int -> Int
updateComponent a b val = case compare a b of
                            LT -> val + 1 
                            GT -> val - 1
                            EQ -> val

updatePos :: Seq.Seq (Moon) -> Seq.Seq (Moon) -> Int-> Seq.Seq (Moon)
updatePos oldMoons newMoons index
    | Seq.null oldMoons = newMoons
    | otherwise = updatePos (Seq.drop 1 oldMoons) (Seq.update index newMoon newMoons) (index + 1)
    where 
        oldMoon = Seq.index oldMoons 0
        newMoon = Moon (xPos oldMoon + xVel oldMoon)
                       (yPos oldMoon + yVel oldMoon)
                       (zPos oldMoon + zVel oldMoon)
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
