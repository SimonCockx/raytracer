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
                   | Compound [SceneObject s]

instance Show (SceneObject s) where
    show (SceneObject a b) = "SceneObject (" ++ show a ++ ") (" ++ show b ++ ")"
    show (SceneLight a) = "SceneLight (" ++ show a ++ ")"
    show (Compound objs) = "Compound " ++ show objs

instance Shape (SceneObject s) where
    intersect ray (SceneObject shape _) = intersect ray shape
    intersect ray (SceneLight shape) = intersect ray shape
    intersect ray (Compound objs) = intersect ray objs
    boundingBox (SceneObject shape _) = boundingBox shape
    boundingBox (SceneLight shape) = boundingBox shape
    boundingBox (Compound objs) = boundingBox objs
    boundingVolume (SceneObject shape _) = boundingVolume shape
    boundingVolume (SceneLight shape) = boundingVolume shape
    boundingVolume (Compound objs) = boundingVolume objs
    numberOfIntersectionTests ray (SceneObject shape _) = numberOfIntersectionTests ray shape
    numberOfIntersectionTests ray (SceneLight shape) = numberOfIntersectionTests ray shape
    numberOfIntersectionTests ray (Compound objs) = numberOfIntersectionTests ray objs


boundSceneObject :: SceneObject s -> SceneObject s
boundSceneObject (SceneObject shape material) = SceneObject (boundingVolume shape) material
boundSceneObject (SceneLight shape) = SceneLight shape
boundSceneObject (Compound objs) = Compound objs


-- | Create a scene object with a white diffuse material and with a specified shape.
simpleObject :: (Shape a, Spectrum s) 
             => a             -- ^ The shape to use
             -> SceneObject s -- ^ The resulting scene object with a white diffuse material
simpleObject shape = SceneObject shape WhiteMaterial


findHit :: (Spectrum s) => Ray Double -> [SceneObject s] -> Maybe (s, BRDF s, Intersection)
findHit ray = foldr (closest (\(_, _, (t, _, _)) -> t) . wrap ray) Nothing
    where
        wrap r obj@(SceneObject _ m) = do
            intersection <- intersect r obj
            return (black, brdf m, intersection)
        wrap r obj@(SceneLight l) = do
            intersection@(t, _, _) <- intersect r obj
            return (getRadiance l (follow r t) (origin r), brdf BlackMaterial, intersection)
        wrap r (Compound objs) = findHit r objs
