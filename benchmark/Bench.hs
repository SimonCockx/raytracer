module Main where

import Criterion.Main

import RayTracer


createScene :: Int -> Int -> Scene RGB
createScene numberOfSpheres numberOfLights =
    let spheres = replicate numberOfSpheres $ simpleObject Sphere
        lights  = replicate numberOfLights $ Light $ PointLight (Point 0 0 0) (white :: RGB)
    in Scene (World spheres lights) camera


camera :: PerspectiveCamera
camera = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)


rayTracer :: SpectrumIndependentRayTracer
rayTracer = SpectrumIndependentRayTracer


gen :: Gen
gen = createGen 29


main :: IO ()
main = do
    let spheresAndLights = [(s, l) | s <- [1], l <- [0, 1, 2]]
    defaultMain [bgroup "ray trace" $ map (\(s, l) -> let name = "spheres=" ++ show s ++ "; lights=" ++ show l in 
        bench name $ nf (render gen rayTracer) $ createScene s l) spheresAndLights]
