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
    -- Cartesian Coords (x, y)
    ray_position_cartesian :: Position,

    -- Polar Coords (r, phi)
    ray_position_polar :: Position,
    ray_velocity_polar :: Direction,

    impact_parameter :: Float,

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

createRay :: Position -> Direction -> Trail -> Float -> RayData
createRay pos d t r_s = 
    let r = sqrt((fst pos) * (fst pos) + (snd pos) * (snd pos))
        phi = atan2 (snd pos) (fst pos)
        
        (dr, dphi) = cartesianToPolarVelocity pos d

        b = if abs dr > 1e-6 
            then r * r * dphi / dr 
            else r * r * dphi * 1e6
        
    in RayData {
        ray_position_cartesian = pos,
        ray_position_polar = (r, phi),
        ray_velocity_polar = (dr, dphi),
        impact_parameter = b,
        direction = d,
        trail = t
    }

cartesianToPolarVelocity :: Position -> Direction -> Direction
cartesianToPolarVelocity (x, y) (dx, dy) =
    let r = sqrt (x * x + y * y)
        dr = (dx * x + dy * y) / r
        dphi = (x * dy - y * dx) / (r * r)
    in (dr, dphi)

geodesic :: Position -> Direction -> Float -> Float -> Direction
geodesic (r, phi) (dr, dphi) r_s b =
    let m = r_s / 2.0
        f = 1.0 - r_s / r
        
        f_safe = max f 0.01

        d2r = if r > r_s * 1.1 then
                  - (m / (r * r)) * (dr * dr) / f_safe
                  + (r - 3.0 * m) * (dphi * dphi) * f_safe
              else
                  0.0
        d2phi = -2.0 * dr * dphi / r
        
    in (d2r, d2phi)

draw :: Model -> Picture
draw []     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position_cartesian bh)) (snd (bh_position_cartesian bh)) $ color red $ circleSolid (radius bh)
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
updateRay (Ray rd) dt model = Ray (createRay newPos newDirection updatedTrail r_s)
    where
    eventHorizons = [radius bh | BlackHole bh <- model]
    r_s = if null eventHorizons then 0 else head eventHorizons

    (r, phi) = ray_position_polar rd
    (dr, dphi) = ray_velocity_polar rd
    b = impact_parameter rd

    (d2r, d2phi) = geodesic (r, phi) (dr, dphi) r_s b

    newdr = dr + d2r * dt * coefficient 
    newdphi = dphi + d2phi * dt * coefficient 

    newr = r + newdr * dt * coefficient
    newPhi = phi + newdphi * dt * coefficient

    currentPos = ray_position_cartesian rd
    
    newPos | any (\eventhorizon -> r <= eventhorizon) eventHorizons = currentPos
           | newr <= r_s = currentPos
           | otherwise = (cos(newPhi) * newr, sin(newPhi) * newr)

    newDirection = (cos(newPhi) * newdr - newr * sin(newPhi) * newdphi,
                    sin(newPhi) * newdr + newr * cos(newPhi) * newdphi)

    updatedTrail = (trail rd) ++ [currentPos]

lightRow :: Int -> Float -> Model
lightRow n r_s = [Ray (createRay (-500, -400 + 30 * fromIntegral x) (1, 0) [] r_s) | x <- [0..n]]
 
initial :: Model
initial = 
    let
        m = 50.0
        bh  = createBlackHole (0, 0) m
        r_s = radius bh
    in BlackHole bh : lightRow 25 r_s

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) black 30 initial draw update