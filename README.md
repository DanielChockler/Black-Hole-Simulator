# Black Hole Simulator

A simulation of light ray paths affected by black holes using geodesic calculations in curved space-time.

## About

This simulator models how black holes warp space-time and alter the path of light rays. Since light is massless, its trajectory is determined by geodesic paths through curved space-time rather than gravitational attraction.

## Getting Started

**Clone the repository:**
```console
git clone https://github.com/DanielChockler/Black-Hole-Simulator
cd Black-Hole-Simulator
```

**Install dependencies:**
```console
cabal install gloss
```

**Run the simulation:**
```console
ghc -O2 -threaded -dynamic main.hs
./main
```

## Features
- Single and multiple black hole simulations
- Real-time visualization using Gloss
- Geodesic path calculations for light rays
