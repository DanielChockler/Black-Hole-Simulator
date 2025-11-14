import Graphics.Gloss

{-
 _   _           _        _ _   ____  _            _      _   _       _        ____  _                 _       _             
| | | | __ _ ___| | _____| | | | __ )| | __ _  ___| | __ | | | | ___ | | ___  / ___|(_)_ __ ___  _   _| | __ _| |_ ___  _ __ 
| |_| |/ _` / __| |/ / _ \ | | |  _ \| |/ _` |/ __| |/ / | |_| |/ _ \| |/ _ \ \___ \| | '_ ` _ \| | | | |/ _` | __/ _ \| '__|
|  _  | (_| \__ \   <  __/ | | | |_) | | (_| | (__|   <  |  _  | (_) | |  __/  ___) | | | | | | | |_| | | (_| | || (_) | |   
|_| |_|\__,_|___/_|\_\___|_|_| |____/|_|\__,_|\___|_|\_\ |_| |_|\___/|_|\___| |____/|_|_| |_| |_|\__,_|_|\__,_|\__\___/|_|   


by Daniel Chockler

Dependancies:

    Gloss
    https://hackage.haskell.org/package/gloss

    to install:
    cabal install gloss

How to run:
    Compile with
    ghc -O2 -threaded -dynamic main.hs

    run
    ./main

Uncomment and change values below to create different simulations

-}

-- Controls simulation speed (e.g. playback_rate = 0.5 for half speed)
playback_rate :: Float
playback_rate = 1.3

-- Arbitrary coefficant: increase to increase the speed of light rays
-- Better not to change this value
coefficient :: Float
coefficient = 80

-- x and y of where the light rays for lightRow begin
lightRowBegin = (-400, -365)

-- x and y of where the light rays for lightFromSinglePoint begin
lightPointBegin = (-800, -550)

-- Single black hole simulation with light rays coming as a row
simulation = singleBlackHole ++ lightRow 40

-- Single black hole simulation with light rays coming from a single point
-- simulation = singleBlackHole ++ lightFromSinglePoint 30

-- Two black hole simulation with light rays coming as a row

-- Black holes horizontally next to each other
-- simulation = binaryBlackHoles1 ++ lightRow 30

-- Black holes vertically next to each other
-- simulation = binaryBlackHoles2 ++ lightRow 30

-- Two black hole simulation with light rays coming from a single point
-- Black holes horizontally next to each other
-- simulation = binaryBlackHoles1 ++ lightFromSinglePoint 30

-- Black holes vertically next to each other
-- simulation = binaryBlackHoles2 ++ lightFromSinglePoint 30

-- Three black hole simulation with light rays coming as a row

-- simulation = threeBlackHoles ++ lightRow 30

-- Three black hole simulation with light rays coming from a single point
-- simulation = threeBlackHoles ++ lightFromSinglePoint 30

--------------------------------------------------------------------------------------------------------------------------------------------------------
-- Don't change below

type Position = (Float, Float)
type Direction = (Float, Float)
type Trail = [Position]
type Mass = Float

data BlackHoleData = BlackHoleData {
    bh_position_cartesian :: Position,
    bh_position_polar :: Position,
    mass :: Mass,
    radius :: Float
}

data RayData = RayData {
    ray_position_cartesian :: Position,
    direction :: Direction,
    trail :: [Position]
}

data Object = Ray RayData | BlackHole BlackHoleData

type Model = [Object]

createBlackHole :: Position -> Mass -> BlackHoleData
createBlackHole pos m = BlackHoleData {
    bh_position_cartesian = pos,
    bh_position_polar = (0, 0),
    mass = m,
    radius = 2.0 * m
}

createRay :: Position -> Direction -> Trail -> RayData
createRay pos d t = RayData {
    ray_position_cartesian = pos,
    direction = d,
    trail = t
}

deflectionFromBlackHole :: Position -> Direction -> BlackHoleData -> (Float, Float)
deflectionFromBlackHole rayPos (dx, dy) bh =
    let bhPos = bh_position_cartesian bh
        m = mass bh
        r_s = radius bh
        
        (rel_x, rel_y) = (fst rayPos - fst bhPos, snd rayPos - snd bhPos)
        r = sqrt (rel_x * rel_x + rel_y * rel_y)
        
        r_safe = max r (r_s * 1.01) 
        
        (unit_x, unit_y) = (rel_x / r_safe, rel_y / r_safe)
        
        f = 1.0 - r_s / r_safe
        gr_factor = (1.0 + r_s / r_safe) / max f 0.01 
        
        accel_magnitude = - (m / (r_safe * r_safe)) * gr_factor
        
        ax = accel_magnitude * (unit_x)
        ay = accel_magnitude * (unit_y)
        
    in (ax, ay)

draw :: Model -> Picture
draw []     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position_cartesian bh)) (snd (bh_position_cartesian bh)) $ color black $ circleSolid (radius bh)
    drawObject (Ray ray)      = pictures ([translate (fst (ray_position_cartesian ray)) (snd (ray_position_cartesian ray)) $ color white $ circleSolid 1] ++ drawTrail (reverse (trail ray)))
        where
        drawTrail :: Trail -> [Picture]
        drawTrail positions | length positions < 2 = []
                            | otherwise            = zipWith3 (drawSegment (length positions)) (init positions) (tail positions) [0..]

        drawSegment totalLength pos1 pos2 i =
            let alpha = fromIntegral i / fromIntegral totalLength
            in color (makeColor 1.0 1.0 1.0 alpha) $ line [pos1, pos2]

update vp dt model = map updateObject model
    where
    updateObject (Ray rd)       = updateRay (Ray rd) (dt * playback_rate) model
    updateObject (BlackHole bh) = (BlackHole bh)

updateRay :: Object -> Float -> Model -> Object
updateRay (Ray rd) dt model = Ray (createRay newPos newDirection updatedTrail)
    where
    blackHoles = [bh | BlackHole bh <- model]
    
    currentPos = ray_position_cartesian rd
    (dx, dy) = direction rd
    
    accelerations = map (deflectionFromBlackHole currentPos (dx, dy)) blackHoles
    (total_ax, total_ay) = foldl (\(ax1, ay1) (ax2, ay2) -> (ax1 + ax2, ay1 + ay2)) (0.0, 0.0) accelerations
    
    newdx = dx + total_ax * dt * coefficient
    newdy = dy + total_ay * dt * coefficient
    
    (x, y) = currentPos
    newx = x + newdx * dt * coefficient
    newy = y + newdy * dt * coefficient
    
    insideHorizon = any (\bh -> 
        let (rel_x, rel_y) = (fst currentPos - fst (bh_position_cartesian bh), 
                               snd currentPos - snd (bh_position_cartesian bh))
            r = sqrt (rel_x * rel_x + rel_y * rel_y)
        in r <= radius bh) blackHoles
    
    newPos | insideHorizon = currentPos
           | otherwise = (newx, newy)
    
    newDirection = (newdx, newdy)
    updatedTrail = take 150 (currentPos : trail rd)

lightRow :: Int -> Model
lightRow n = [Ray (createRay (fst lightRowBegin, snd lightRowBegin + 20 * fromIntegral x) (1, 0) []) | x <- [0..n]]

lightFromSinglePoint :: Int -> Model
lightFromSinglePoint n = 
    let step = (pi / 3) / fromIntegral (n - 1)
        angles = [-pi/18 + step * fromIntegral i | i <- [0..n-1]]
    in [Ray (createRay lightPointBegin (cos (angle), sin (angle)) []) | angle <- angles]

singleBlackHole :: Model
singleBlackHole = [BlackHole (createBlackHole (0, 0) 50)]

binaryBlackHoles1 :: Model
binaryBlackHoles1 = [BlackHole (createBlackHole (-200, 0) 50), BlackHole (createBlackHole (200, 0) 50)]

binaryBlackHoles2 :: Model
binaryBlackHoles2 = [BlackHole (createBlackHole (0, 200) 50), BlackHole (createBlackHole (0, -200) 50)]

threeBlackHoles :: Model
threeBlackHoles = [BlackHole (createBlackHole (-400, -200) 40), BlackHole (createBlackHole (0, 0) 50), BlackHole (createBlackHole (200, -300) 30)]

initial :: Model
initial = simulation

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) (makeColorI 23 8 41 0) 200 initial draw update