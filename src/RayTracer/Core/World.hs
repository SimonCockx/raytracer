module RayTracer.Core.World
    ( World (objects, lights)
    , createWorld
    , insertBoundingBoxes
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

data World s = World {objects :: [SceneObject s], lights :: [Light s]}
    deriving (Show)

createWorld :: (Spectrum s) => [SceneObject s] -> [Light s] -> World s
createWorld os ls = World (map SceneLight ls ++ os) ls

instance Shape (World s) where
    intersect ray (World os _) = intersect ray os
    boundingBox = boundingBox . objects
    boundingVolume = boundingVolume . objects
    numberOfIntersectionTests ray = numberOfIntersectionTests ray . objects

insertBoundingBoxes :: World s -> World s
insertBoundingBoxes world = world {objects = map boundSceneObject $ objects world}
