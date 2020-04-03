{-# LANGUAGE TypeFamilies #-}

module RayTracer.Core.World
    ( World (..)
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

data World s = World {objects :: [SceneObject s], lights :: [Light s]}
    deriving (Show)

instance Boundable (World s) where
    boundingBox = boundingBox . objects
instance Shape (World s) where
    intersect ray = intersect ray . objects
    numberOfIntersectionTests ray = numberOfIntersectionTests ray . objects
    boundedNode = boundedNode . objects
instance (s1 ~ s2) => Object (World s1) s2 where
    findHit ray = findHit ray . objects
    boundedObjectNode = boundedObjectNode . objects
