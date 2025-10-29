import Graphics.Gloss
import System.Random
import System.IO.Unsafe

g :: Float
g = 6.6743e-11

c :: Float
c = 3.0e8

type Position = (Float, Float)
type Velocity = (Float, Float)
type Mass = Float

data BlackHoleData = BlackHoleData {
    bh_position :: Position,
    mass :: Mass,
    radius :: Float
}

data RayData = RayData {
	ray_position :: Position,
    velocty :: Velocity
}

data Object = Ray RayData | BlackHole BlackHoleData

type Model = [Object]

createBlackHole :: Position -> Mass -> BlackHoleData
createBlackHole pos m = BlackHoleData {
    bh_position = pos,
    mass = m,
    radius = (2 * g * m) / (c ** 2)
}

createRay :: Position -> Velocity -> RayData
createRay pos v = RayData {
    velocty = v,
	ray_position = pos
}


draw :: Model -> Picture
draw []                     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position bh)) (snd (bh_position bh)) $ color red $ circleSolid (radius bh)
    drawObject (Ray ray)      = translate (fst (ray_position ray)) (snd (ray_position ray)) $ color white $ circleSolid 5

blackhole :: Object
blackhole = BlackHole (createBlackHole (100, 100) 5.39e28)

ray :: Object
ray = Ray (createRay (0, 0) (10, 10))

main :: IO ()
main = display (InWindow "Window" (1500, 1500) (0, 0)) black (draw [blackhole, ray])

