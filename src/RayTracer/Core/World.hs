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

instance Boundable (World s) where
    type BoundedContent (World s) = SceneObject_ s
    boundingBox = boundingBox . objects
    boundingVolume = boundingVolume . objects
instance Shape (World s) where
    intersect ray = intersect ray . objects
    numberOfIntersectionTests ray = numberOfIntersectionTests ray . objects
instance (s1 ~ s2) => SceneObject_ s2 (World s1) where
    findHit ray = findHit ray . objects
