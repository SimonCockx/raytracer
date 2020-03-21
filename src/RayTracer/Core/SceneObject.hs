{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RayTracer.Core.SceneObject
    ( SceneObject_ (..)
    , SceneObject (..)
    , Surface (..)
    , WhiteShape (..)
    , simpleObject
    , withMaterial
    ) where

import RayTracer.Geometry
import RayTracer.Lightning

import Data.Maybe
import Data.List hiding (intersect)

class (Shape a) => SceneObject_ a s where
    findHit :: Ray Double -> a -> Maybe (s, BRDF s, Intersection)


data SceneObject s = forall a. (SceneObject_ a s, Show a) => SceneObject a

instance Show (SceneObject s) where
    show (SceneObject surface) = show surface
instance Shape (SceneObject s) where
    intersect ray (SceneObject obj) = intersect ray obj
    boundingBox (SceneObject obj) = boundingBox obj
    --boundingVolume (SceneObject obj) = SceneObject $ boundingVolume obj -- TODO: nu gaat hij een dubbele box plaatsen rond elk scene object?
    numberOfIntersectionTests ray (SceneObject obj) = numberOfIntersectionTests ray obj
instance (a ~ s) => SceneObject_ (SceneObject s) a where
    findHit ray (SceneObject surface) = findHit ray surface


closestHit :: Maybe (s, BRDF s, Intersection) -> Maybe (s, BRDF s, Intersection) -> Maybe (s, BRDF s, Intersection)
closestHit = closest (\(_, _, (t, _, _)) -> t)
instance (SceneObject_ obj s, Shape (BoundedShape obj)) => SceneObject_ [obj] s where
    findHit ray = foldr (closestHit . findHit ray) Nothing


data Surface a s = Surface a (Material s)

instance (Show a) => Show (Surface a s) where
    show (Surface shape _) = "Surface (" ++ show shape ++ ")"
instance (Shape a) => Shape (Surface a s) where
    intersect ray (Surface shape _) = intersect ray shape
    boundingBox (Surface shape _) = boundingBox shape
    --boundingVolume (Surface shape _) = boundingVolume shape -- TODO: zie scene object
    numberOfIntersectionTests ray (Surface shape _) = numberOfIntersectionTests ray shape
instance (Shape a, Spectrum s) => SceneObject_ (Surface a s) s where
    findHit ray (Surface shape getBRDF) = do
        intersection@(_, _, uvw) <- intersect ray shape
        return (black, getBRDF uvw, intersection)
instance (Transformable a b) => Transformable (Surface a s) b where
    transform t (Surface shape brdf) = Surface (t `transform` shape) brdf


instance (Spectrum s) => SceneObject_ (Light s) s where
    findHit ray light = do
        intersection@(t, _, _) <- intersect ray light
        return (getRadiance light (follow ray t) (origin ray), blackBRDF, intersection)


newtype WhiteShape a = WhiteShape a
    deriving (Show)
deriving instance (Shape a) => Shape (WhiteShape a) 
instance (Spectrum s, Shape a) => SceneObject_ (WhiteShape a) s where
    findHit ray shape = do
        intersection <- intersect ray shape
        return (black, whiteBRDF, intersection)


simpleObject :: (Shape a, Spectrum s) => a -> SceneObject s
simpleObject shape = SceneObject $ WhiteShape shape

withMaterial :: (Shape a, Spectrum s) => a -> Material s -> SceneObject s
withMaterial shape = SceneObject . Surface shape


instance (SceneObject_ a s) => SceneObject_ (TransformedShape a) s where
    findHit ray (Transformed m obj) = do
        (spec, brdf, (t, n, uvw)) <- findHit (inverseTransform m ray) obj
        return (spec, brdf, (t, normalTransform m n, uvw))

instance (SceneObject_ a s) => SceneObject_ (BoundingVolume a) s where
    findHit ray obj = case intersect ray $ boundingBox obj of
        Nothing -> Nothing
        Just _  -> innerHit ray obj

innerHit :: (SceneObject_ a s) => Ray Double -> BoundingVolume a -> Maybe (s, BRDF s, Intersection)
innerHit ray (Bounded _ innerShape) = findHit ray innerShape
innerHit ray (TransformedBoundingVolume _ m innerShape) = do
    (spec, brdf, (t, n, uvw)) <- innerHit (inverseTransform m ray) innerShape
    return (spec, brdf, (t, normalTransform m n, uvw))
innerHit ray (BoundingVolume _ innerVolumes) = intersectVts volumesTs
    where
        volumesTs = sortOn fst
                  $ mapMaybe (\innerVolume -> fmap (\(t, _, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerHit ray innerVolume of
            Nothing     -> intersectVts vts
            Just (spec, brdf, (t, n, uvw)) -> foldr (closestHit . innerHit ray . snd) (Just (spec, brdf, (t, n, uvw))) $ takeWhile ((<t) . fst) vts
