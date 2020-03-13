module RayTracer.Core.World
    ( World (..)
    , insertBoundingBoxes
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

data World s = World {objects :: [SceneObject s], lights :: [Light s]}
    deriving (Show)

instance (Show s) => Shape (World s) where
    -- intersect ray (World objects lights) = closestIntersection objectIntersection lightIntersection
    --     where
    --         objectIntersection = intersect ray objects
    --         lightIntersection  = intersect ray lights
    intersect ray (World objects lights) = intersect ray objects
    boundingBox = boundingBox . objects
    boundingVolume = boundingVolume . objects
    numberOfIntersectionTests ray = numberOfIntersectionTests ray . objects

insertBoundingBoxes :: World s -> World s
insertBoundingBoxes world = world {objects = map boundSceneObject $ objects world}
