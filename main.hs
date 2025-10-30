import Graphics.Gloss
import System.Random
import System.IO.Unsafe

playback_rate :: Float
playback_rate = 1.0

g :: Float
g = 6.6743e-11

c :: Float
c = 3.0e8

type Position = (Float, Float)
type Direction = (Float, Float)
type Mass = Float

data BlackHoleData = BlackHoleData {
    bh_position :: Position,
    mass :: Mass,
    radius :: Float
}

data RayData = RayData {
	ray_position :: Position,
    direction :: Direction
}

data Object = Ray RayData | BlackHole BlackHoleData

type Model = [Object]

createBlackHole :: Position -> Mass -> BlackHoleData
createBlackHole pos m = BlackHoleData {
    bh_position = pos,
    mass = m,
    radius = (2 * g * m) / (c ** 2)
}

createRay :: Position -> Direction -> RayData
createRay pos d = RayData {
    ray_position = pos,
    direction = d
}

draw :: Model -> Picture
draw []                     = Blank
draw (x:xs) = pictures (drawObject x : [draw xs])
    where
    drawObject (BlackHole bh) = translate (fst (bh_position bh)) (snd (bh_position bh)) $ color red $ circleSolid (radius bh)
    drawObject (Ray ray)      = translate (fst (ray_position ray)) (snd (ray_position ray)) $ color white $ circleSolid 5

update vp dt model = [updateRay (model !! 1) (dt * playback_rate) model]

updateRay :: Object -> Float -> Model -> Object
updateRay (Ray rd) dt model = Ray (createRay ((fst (ray_position rd)) + ((fst (direction rd)) * c)), (snd (ray_position rd) + ((snd (direction rd)) * c)), (direction rd))

initial :: Model
initial = [BlackHole (createBlackHole (100, 100) 5.39e28), Ray (createRay (100, 10) (1, 0))]

main :: IO ()
main = simulate (InWindow "Window" (1500, 1500) (0, 0)) black 30 initial draw update

