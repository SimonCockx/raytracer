module Main where

import Criterion.Main

import RayTracer
import Data.Massiv.Array hiding (map, replicate)
import Control.Scheduler (getWorkerId)

seeds :: [Seed]
seeds = [11, 23, 55, 83, 145, 250, 954, 1010]

getWorkerGens :: IO (WorkerStates Gen)
getWorkerGens = initWorkerStates Par $ createGen . (seeds !!) . getWorkerId

createScene :: Int -> Int -> Scene RGB
createScene numberOfSpheres numberOfLights =
    let spheres = replicate numberOfSpheres $ simpleObject Sphere
        lights  = replicate numberOfLights $ Light $ PointLight (Point 0 0 0) (white :: RGB)
    in Scene (createWorld spheres lights) camera


camera :: PerspectiveCamera
camera = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)


rayTracer :: DirectLightningTracer
rayTracer = DirectLightningTracer (Random 1)


main :: IO ()
main = do
    gens <- getWorkerGens
    let spheresAndLights = [(s, l) | s <- [1], l <- [0, 1, 2]]
    defaultMain [bgroup "ray trace" $ map (\(s, l) -> let name = "spheres=" ++ show s ++ "; lights=" ++ show l in 
        bench name $ nf (render gens rayTracer) $ createScene s l) spheresAndLights]
