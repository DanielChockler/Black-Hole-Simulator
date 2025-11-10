import Graphics.Gloss
import System.Random
import System.IO.Unsafe

playback_rate :: Float
playback_rate = 1.0

-- arbitrary coefficant: increase to increase the speed of light rays
coefficient :: Float
coefficient = 200

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
        
        r_safe = max r (r_s * 1.1)
        
        (unit_x, unit_y) = (rel_x / r_safe, rel_y / r_safe)
        
        f = 1.0 - r_s / r_safe
        gr_factor = (1.0 + r_s / r_safe) / max f 0.1
        
        accel_magnitude = if r > r_s * 1.1 then
                              - (m / (r_safe * r_safe)) * gr_factor
                          else
                              0.0
        
        ax = accel_magnitude * (unit_x)
        ay = accel_magnitude * (unit_y)
        
    in (ax, ay)

draw :: Model -> Picture
draw []     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position_cartesian bh)) (snd (bh_position_cartesian bh)) $ color black $ circleSolid (radius bh)
    drawObject (Ray ray)      = pictures ([translate (fst (ray_position_cartesian ray)) (snd (ray_position_cartesian ray)) $ color white $ circleSolid 1] ++ drawTrail (reverse (take 100 (reverse (trail ray)))))
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
    updatedTrail = (trail rd) ++ [currentPos]

lightRow :: Int -> Model
lightRow n = [Ray (createRay (-500, -550 + 30 * fromIntegral x) (1, 0) []) | x <- [0..n]]

initial :: Model
initial = 
    let
        m1 = 50.0
        m2 = 30.0
        m3 = 40.0
        
        bh1 = createBlackHole (0, 0) m1
        bh2 = createBlackHole (300, 200) m2
        bh3 = createBlackHole (-200, -100) m3
        
    in [BlackHole bh1, BlackHole bh2, BlackHole bh3] ++ lightRow 25

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) (makeColorI 23 8 41 0) 30 initial draw update

