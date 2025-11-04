import Graphics.Gloss
import System.Random
import System.IO.Unsafe

playback_rate :: Float
playback_rate = 1.0

coefficient :: Float
coefficient = 5.0e-7

g :: Float
g = 6.6743e-11

c :: Float
c = 3.0e8

r_s :: Float
r_s = (2 * g * 5.39e28) / (c ** 2)

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
    ray_position_polar :: Position,
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

createRay :: Position -> Direction -> Trail -> RayData
createRay pos d t = RayData {
    ray_position_cartesian = pos,
    ray_position_polar = (sqrt((fst pos) * (fst pos) + (snd pos) * (snd pos)), atan ((snd pos) / (fst pos))),
    direction = d,
    trail = t
}

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
updateRay (Ray rd) dt model = Ray (createRay newPos (direction rd) updatedTrail)
    where
    currentPos = ray_position_cartesian rd
    currentDir = direction rd
    newPos | fst (ray_position_polar rd) > r_s = (fst currentPos + fst currentDir * c * dt * coefficient,
                                                 snd currentPos + snd currentDir * c * dt * coefficient)
           | otherwise                         = currentPos
    updatedTrail = (trail rd) ++ [currentPos]

lightRow :: Int -> Model
lightRow n = [Ray (createRay (-200, -200 + 25 * fromIntegral x) (1, 0) []) | x <- [0..n]]

initial :: Model
initial = BlackHole (createBlackHole (0, 0) 5.39e28) : lightRow 15

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) black 30 initial draw update

