{-# LANGUAGE TypeFamilies #-}

module RayTracer.Core.World
    ( World (objects, lights)
    , createWorld
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

data World s = World {objects :: [SceneObject s], lights :: [Light s]}
    deriving (Show)

createWorld :: (Spectrum s) => [SceneObject s] -> [Light s] -> World s
createWorld os ls = World (map SceneObject ls ++ os) ls

instance Shape (World s) where
    type BoundedShape (World s) = SceneObject s
    intersect ray (World os _) = intersect ray os
    boundingBox = boundingBox . objects
    boundingVolume = boundingVolume . objects
    numberOfIntersectionTests ray = numberOfIntersectionTests ray . objects
