module RayTracer.Core.World
    ( World (..)
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

data World s = World {objects :: [SceneObject s], lights :: [Light s]}
    deriving (Show)

instance (Show s) => Shape (World s) where
    intersect ray (World objects lights) = closestIntersection objectIntersection lightIntersection
        where
            objectIntersection = intersect ray objects
            lightIntersection  = intersect ray lights
