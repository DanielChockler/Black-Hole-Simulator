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

type Position = (Float, Float)
type Direction = (Float, Float)
type Trail = [Position]
type Mass = Float

data BlackHoleData = BlackHoleData {
    bh_position :: Position,
    mass :: Mass,
    radius :: Float
}

data RayData = RayData {
    ray_position :: Position,
    direction :: Direction,
    trail :: [Position]
}

data Object = Ray RayData | BlackHole BlackHoleData

type Model = [Object]

createBlackHole :: Position -> Mass -> BlackHoleData
createBlackHole pos m = BlackHoleData {
    bh_position = pos,
    mass = m,
    radius = (2 * g * m) / (c ** 2)
}

createRay :: Position -> Direction -> Trail -> RayData
createRay pos d t = RayData {
    ray_position = pos,
    direction = d,
    trail = t
}

draw :: Model -> Picture
draw []                     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position bh)) (snd (bh_position bh)) $ color red $ circleSolid (radius bh)
    drawObject (Ray ray)      = pictures [
                                translate (fst (ray_position ray)) (snd (ray_position ray)) $ color white $ circleSolid 1,
                                color (makeColor 1.0 1.0 1.0 fromIntegral((length t / (length t + 1)))) $ lineLoop t]
                                where
                                t = trail ray

update vp dt model = map updateObject model
    where
    updateObject (Ray rd) = updateRay (Ray rd) (dt * playback_rate) model
    updateObject (BlackHole bh) = (BlackHole bh)

updateRay :: Object -> Float -> Model -> Object
updateRay (Ray rd) dt model = Ray (createRay newPos (direction rd) updatedTrail)
    where
    currentPos = ray_position rd
    currentDir = direction rd
    newPos = (fst currentPos + fst currentDir * c * dt * coefficient,
              snd currentPos + snd currentDir * c * dt * coefficient)
    updatedTrail = (trail rd) ++ [currentPos]

lightRow :: Int -> Model
lightRow n = [Ray (createRay (-200, 25 * fromIntegral x) (1, 0) []) | x <- [0..n]]

initial :: Model
initial = BlackHole (createBlackHole (100, 100) 5.39e28) : lightRow 9

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) black 30 initial draw update

