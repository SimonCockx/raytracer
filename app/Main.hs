module Main where

import RayTracer
import Scene
import Camera
import Shape
import Data.Massiv.Array
import Data.Massiv.Array.IO
import Vector

scene :: Scene
scene = Scene [ Shape $ Sphere (createPoint 0 0 (-10)) 3
              , Shape $ Sphere (createPoint 3 1 (-11)) 2
              ]

camera :: PerspectiveCamera
camera = createPerspectiveCamera 256 256 (createPoint 0 0 0) (createVector 0 0 (-1)) (createVector 0 1 0) (pi/2)

rayTracer :: LinearDepthRayTracer
rayTracer = LinearDepthRayTracer 5 15

main :: IO ()
main = do
    let image = rayTrace rayTracer scene camera
    writeImage "result.jpg" image
    displayImageUsing defaultViewer True image
