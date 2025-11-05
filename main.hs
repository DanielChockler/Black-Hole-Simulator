import Graphics.Gloss
import System.Random
import System.IO.Unsafe

playback_rate :: Float
playback_rate = 1.0

coefficient :: Float
coefficient = 9.0e-10

g :: Float
g = 6.6743e-11

c :: Float
c = 3.0e8

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
    radius = (2 * g * m) / (c ** 2)
}

createRay :: Position -> Direction -> Trail -> Float -> RayData
createRay pos d t r_s = RayData {
    ray_position_cartesian = pos,
    ray_position_polar = (sqrt((fst pos) * (fst pos) + (snd pos) * (snd pos)), atan2 (snd pos) (fst pos)),
    ray_velocity_polar = cartesianToPolarDirection pos d r_s,
    direction = d,
    trail = t
}

cartesianToPolarDirection :: Position -> Direction -> Float -> Direction
cartesianToPolarDirection (x, y) (dx, dy) r_s =
    let r    = sqrt (x * x + y * y)
        phi  = atan2 y x

        dr   = (dx * x + dy * y) / r
        dphi = (dx * (-y) + dy * x) / (r * r)

    in (dr, dphi)

geodesic :: Position -> Direction -> Float -> Direction
geodesic (x, y) (dx, dy) r_s =
    let r    = sqrt (x * x + y * y)
        phi  = atan2 y x

        dr   = (dx * x + dy * y) / r
        dphi = (dx * (-y) + dy * x) / (r * r)

        d2r = r * dphi * dphi - (c * c * r_s) / (2.0 * r * r)
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

    (d2r, d2phi) = geodesic (ray_position_cartesian rd) (direction rd) r_s

    newdr = dr + d2r  * dt * coefficient
    newdphi = dphi + d2phi  * dt * coefficient

    newr = r + newdr * c * dt * coefficient
    newPhi = phi + newdphi * c * dt * coefficient

    currentPos = ray_position_cartesian rd
    newPos | any (\eventhorizon -> r <= eventhorizon) eventHorizons = currentPos
           | otherwise                                              = (cos(newPhi) * newr,
                                                                       sin(newPhi) * newr)

    newDirection = (cos(newPhi) * newdr - newr * sin(newPhi) * newdphi,
                    sin(newPhi) * newdr + newr * cos(newPhi) * newdphi)

    updatedTrail = (trail rd) ++ [currentPos]

lightRow :: Int -> Float -> Model
lightRow n r_s = [Ray (createRay (-500, -400 + 30 * fromIntegral x) (0.5, 0.3) [] r_s) | x <- [0..n]]

initial :: Model
initial = 
    let bh  = createBlackHole (0, 0) 5.39e28
        r_s = radius bh
    in BlackHole bh : lightRow 25 r_s

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) black 30 initial draw update

