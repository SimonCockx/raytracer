{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Core.SceneObject
    ( SceneObject (..)
    , boundSceneObject
    , simpleObject
    , findHit
    ) where

import RayTracer.Geometry
import RayTracer.Lightning

-- | A type that represents an object in a scene with a specific shape and material.
data SceneObject s = forall a b. (Shape a, Show a, Material b s, Show b) => SceneObject a b

instance Show (SceneObject s) where
    show (SceneObject a b) = "SceneObject (" ++ show a ++ ") (" ++ show b ++ ")"

instance Shape (SceneObject s) where
    intersect ray (SceneObject shape _) = intersect ray shape
    boundingBox (SceneObject shape _) = boundingBox shape

instance Material (SceneObject s) s where
    brdf (SceneObject _ mat) = brdf mat


boundSceneObject :: SceneObject s -> SceneObject s
boundSceneObject (SceneObject shape material) = SceneObject (boundingVolume shape) material


-- | Create a scene object with a white diffuse material and with a specified shape.
simpleObject :: (Show a, Shape a, Spectrum s) 
             => a             -- ^ The shape to use
             -> SceneObject s -- ^ The resulting scene object with a white diffuse material
simpleObject shape = SceneObject shape SimpleMaterial


findHit :: Ray Double -> [SceneObject s] -> Maybe (SceneObject s, Intersection)
findHit ray = foldr (closest (\(_, (t, _)) -> t) . wrap ray) Nothing
    where
        wrap ray obj@(SceneObject shape _) = do
            intersection <- intersect ray shape
            return (obj, intersection)
