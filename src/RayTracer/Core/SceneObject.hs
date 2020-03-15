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
                   | forall a. (LightSource a s, Show a) => SceneLight a

instance Show (SceneObject s) where
    show (SceneObject a b) = "SceneObject (" ++ show a ++ ") (" ++ show b ++ ")"
    show (SceneLight a) = "SceneLight (" ++ show a ++ ")"

instance Shape (SceneObject s) where
    intersect ray (SceneObject shape _) = intersect ray shape
    intersect ray (SceneLight shape) = intersect ray shape
    boundingBox (SceneObject shape _) = boundingBox shape
    boundingBox (SceneLight shape) = boundingBox shape
    boundingVolume (SceneObject shape _) = boundingVolume shape
    boundingVolume (SceneLight shape) = boundingVolume shape
    numberOfIntersectionTests ray (SceneObject shape _) = numberOfIntersectionTests ray shape
    numberOfIntersectionTests ray (SceneLight shape) = numberOfIntersectionTests ray shape

instance (Spectrum s) => Material (SceneObject s) s where
    brdf (SceneObject _ mat) a b c = brdf mat a b c
    brdf _ _ _ _ = black

instance (Spectrum s) => LightSource (SceneObject s) s where
    getRadiance (SceneObject _ _) _ _ = black
    getRadiance (SceneLight l) a b = getRadiance l a b
    generateSample _ (SceneObject _ _) _ = return []
    generateSample strat (SceneLight l) a = generateSample strat l a


boundSceneObject :: SceneObject s -> SceneObject s
boundSceneObject (SceneObject shape material) = SceneObject (boundingVolume shape) material
boundSceneObject (SceneLight shape) = SceneLight shape


-- | Create a scene object with a white diffuse material and with a specified shape.
simpleObject :: (Shape a, Spectrum s) 
             => a             -- ^ The shape to use
             -> SceneObject s -- ^ The resulting scene object with a white diffuse material
simpleObject shape = SceneObject shape WhiteMaterial


findHit :: Ray Double -> [SceneObject s] -> Maybe (SceneObject s, Intersection)
findHit ray = foldr (closest (\(_, (t, _)) -> t) . wrap ray) Nothing
    where
        wrap r obj = do
            intersection <- intersect r obj
            return (obj, intersection)
