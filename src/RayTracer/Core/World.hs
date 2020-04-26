module RayTracer.Core.World
    ( World (..)
    , createWorld
    , createWorldWithColor
    ) where

import RayTracer.Core.SceneObject
import RayTracer.Lightning
import RayTracer.Geometry

type Background s = Vector Double -> s
data World s = forall a. (Object a s, Show a) => World {worldRoot :: a, worldLights :: [Light s], worldBackground :: Background s}

createWorld :: (Spectrum s, Object a s) => a -> [Light s] -> World s
createWorld root lights = createWorldWithColor root lights black

createWorldWithColor :: (Object a s) => a -> [Light s] -> s -> World s
createWorldWithColor root lights color = World root lights $ const color

instance Show (World s) where
    show (World root lights _) = "World {worldRoot = " ++ show root ++ ", worldLights = " ++ show lights ++ "}"
instance Shape (World s) where
    intersect ray World{worldRoot = root} = intersect ray root
    numberOfIntersectionTests ray World{worldRoot = root} = numberOfIntersectionTests ray root
    boundingBox World{worldRoot = root} = boundingBox root
    boundedNode World{worldRoot = root} = boundedNode root
instance (s1 ~ s2) => Object (World s1) s2 where
    findHit ray World{worldRoot = root} = findHit ray root
    boundedObjectNode World{worldRoot = root} = boundedObjectNode root
