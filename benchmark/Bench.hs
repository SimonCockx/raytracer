module Main where

import Criterion.Main

import Scene
import Camera
import RayTracer
import Vector
import Shape

import Data.Massiv.Array as A

scene :: Scene
scene = Scene [ Shape $ Sphere (Point 0 0 (-10)) 3
              , Shape $ Sphere (Point 3 1 (-11)) 2
              ]

camera :: PerspectiveCamera
camera = createPerspectiveCamera 256 256 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2)

camWithRes res = createPerspectiveCamera res res (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2)

rayTracer :: LinearDepthRayTracer
rayTracer = LinearDepthRayTracer 5 15

main :: IO ()
main = defaultMain [
    bgroup "ray trace" [ bench "linear depth 16" $ nf (rayTrace rayTracer scene) (camWithRes 16)
                       , bench "linear depth 32" $ nf (rayTrace rayTracer scene) (camWithRes 32)
                       , bench "linear depth 64" $ nf (rayTrace rayTracer scene) (camWithRes 64)
                       , bench "linear depth 128" $ nf (rayTrace rayTracer scene) (camWithRes 128)
                       , bench "linear depth 256" $ nf (rayTrace rayTracer scene) (camWithRes 256)
                       ]
    ]