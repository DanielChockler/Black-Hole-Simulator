# Black Hole Simulation

## How the simulation was developed
Simulating how the path of light rays are affected by a single or multiple black holes.
Since light is without mass, the path of light cannot be determined by gravitational attraction, instead the path of light is altered by the warping of space-time due to the black hole.
Therefore the path of the light rays was found by calculating the geodesic path of the light rays

## How to run
Clone the repository
```console
foo@bar:~$ git clone https://github.com/DanielChockler/Black-Hole-Simulator
foo@bar:~$ cd Black-Hole-Simulator
```

Install dependencies
```console
foo@bar:~$ cabal install gloss
```

Compile and run code
```console
foo@bar:~$ ghc -O2 -threaded -dynamic main.hs
foo@bar:~$ ./main
```
