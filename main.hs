import Graphics.Gloss
import System.Random
import System.IO.Unsafe

g :: Float
g = 6.6743e-11

c :: Float
c = 3.0e8

type Position = (Float, Float)
type Mass = Float

data BlackHole = BlackHole {
    position :: Position,
    mass :: Mass,
    radius :: Float
}

data Ray = Ray {
	position :: Position
}

data Object = Ray | BlackHole

type Model = [Object]

createBlackHole :: Position -> Mass -> BlackHole
createBlackHole pos m = BlackHole {
    position = pos,
    mass = m,
    radius = (2 * g * m) / (c ** 2)
}

window :: Display
window = InWindow "Black Hole" (1500, 1000) (0, 0)

background :: Color
background = black

draw :: Model -> Picture
draw [x]    | isBlackHole x = _
            | isRay x       = _
draw (x:xs) | isBlackHole x = _
            | isRay x       = _
    where
    isBlackHole 

blackhole :: BlackHole
blackhole = createBlackHole (100, 100) 5.39e28

main :: IO ()
main = display window background (draw [blackhole])