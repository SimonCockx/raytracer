{-# OPTIONS_GHC -w #-}

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


class (Shape a, Boundable a, SceneObject_ s ~ BoundedContent a) => SceneObject_ s a where
    findHit :: Ray Double -> a -> Maybe (s, BRDF s, Intersection)


data SceneObject s = forall a. (SceneObject_ s a, Show a) => SceneObject a

instance Show (SceneObject s) where
    show (SceneObject obj) = "SceneObject (" ++ show obj ++ ")"
instance Boundable (SceneObject s) where
    type BoundedContent (SceneObject s) = SceneObject_ s
    boundingBox (SceneObject obj) = boundingBox obj
    boundingVolume (SceneObject obj) = boundingVolume obj
instance Shape (SceneObject s) where
    intersect ray (SceneObject obj) = intersect ray obj
    numberOfIntersectionTests ray (SceneObject obj) = numberOfIntersectionTests ray obj
instance (s1 ~ s2) => SceneObject_ s2 (SceneObject s1) where
    findHit ray (SceneObject surface) = findHit ray surface



closestHit :: Maybe (s, BRDF s, Intersection) -> Maybe (s, BRDF s, Intersection) -> Maybe (s, BRDF s, Intersection)
closestHit = closest (\(_, _, (t, _, _)) -> t)
instance (SceneObject_ s obj) => SceneObject_ s [obj] where
    findHit ray = foldr (closestHit . findHit ray) Nothing


data Surface s a = Surface a (Material s)

instance (Show a) => Show (Surface s a) where
    show (Surface shape _) = "Surface (" ++ show shape ++ ")"
instance (Shape a, Boundable a, Shape ~ BoundedContent a, Spectrum s) => Boundable (Surface s a) where
    type BoundedContent (Surface s a) = SceneObject_ s
    boundingBox (Surface shape _) = boundingBox shape
    boundingVolume surface@(Surface shape _) = SubtreeNode surface $ boundingVolume shape
instance (Shape a) => Shape (Surface s a) where
    intersect ray (Surface shape _) = intersect ray shape
    numberOfIntersectionTests ray (Surface shape _) = numberOfIntersectionTests ray shape
instance (Shape a, Boundable a, Shape ~ BoundedContent a, Spectrum s1, s1 ~ s2) => SceneObject_ s2 (Surface s1 a) where
    findHit ray (Surface shape getBRDF) = do
        intersection@(_, _, uvw) <- intersect ray shape
        return (black, getBRDF uvw, intersection)
instance (Transformable a b) => Transformable (Surface s a) b where
    transform t (Surface shape brdf) = Surface (t `transform` shape) brdf


instance Boundable (Light s) where
    type BoundedContent (Light s) = SceneObject_ s
    boundingBox (Light l) = boundingBox l
    boundingVolume light@(Light l) = SubtreeNode light $ boundingVolume l
instance (Spectrum s1, s1 ~ s2) => SceneObject_ s2 (Light s1) where
    findHit ray (Light l) = do
        intersection@(t, _, _) <- intersect ray l
        return (getRadiance l (follow ray t) (origin ray), blackBRDF, intersection)


newtype WhiteShape s a = WhiteShape a
    deriving (Show)
deriving instance (Shape a) => Shape (WhiteShape s a)
instance (Shape a, Boundable a, Shape ~ BoundedContent a, Spectrum s) => Boundable (WhiteShape s a) where
    type BoundedContent (WhiteShape s a) = SceneObject_ s
    boundingBox (WhiteShape shape) = boundingBox shape
    boundingVolume whiteShape@(WhiteShape shape) = SubtreeNode whiteShape $ boundingVolume shape
instance (Spectrum s1, s1 ~ s2, Shape a, Boundable a, Shape ~ BoundedContent a) => SceneObject_ s2 (WhiteShape s1 a) where
    findHit ray (WhiteShape shape) = do
        intersection <- intersect ray shape
        return (black, whiteBRDF, intersection)


simpleObject :: (Shape a, Boundable a, Shape ~ BoundedContent a, Spectrum s) => a -> SceneObject s
simpleObject = SceneObject . WhiteShape

withMaterial :: (Shape a, Boundable a, Shape ~ BoundedContent a, Spectrum s) => a -> Material s -> SceneObject s
withMaterial shape = SceneObject . Surface shape


instance (SceneObject_ s a) => SceneObject_ s (Transformed a) where
    findHit ray (Transformed m obj) = do
        (spec, brdf, (t, n, uvw)) <- findHit (inverseTransform m ray) obj
        return (spec, brdf, (t, normalTransform m n, uvw))
-- data BoundingObject s = BoundingObject AABB [BoundingObject s]
--                       | forall a. (SceneObject_ a s, Shape a, Show a) => BoundedObject AABB a
--                       | TransformedBoundingObject AABB (Transformation Double) (BoundingObject s)
-- instance Show (BoundingObject s) where
--     show (BoundingObject box innerVolumes) = "BoundingObject (" ++ show box ++ ") " ++ show innerVolumes
--     show (BoundedObject box obj) = "BoundedObject (" ++ show box ++ ") (" ++ show obj ++ ")"
--     show (TransformedBoundingObject box t volume) = "TransformedBoundingObject (" ++ show box ++ ") (" ++ show t ++ ") (" ++ show volume ++ ")" 
-- instance Shape (BoundingObject s) where
--     intersect ray volume = intersect ray $ boundingVolume volume
--     boundingBox volume = boundingBox $ boundingVolume volume
--     boundingVolume (BoundingObject box innerVolumes) = BoundingVolume box $ map boundingVolume innerVolumes
--     boundingVolume (BoundedObject box obj) = Bounded box obj
--     boundingVolume (TransformedBoundingObject box t obj) = TransformedBoundingVolume box t $ boundingVolume obj
--     numberOfIntersectionTests ray volume = numberOfIntersectionTests ray $ boundingVolume volume
-- instance (s1 ~ s2) => SceneObject_ (BoundingObject s1) s2 where
--     findHit ray obj = case intersect ray $ boundingBox obj of
--         Nothing -> Nothing
--         Just _  -> innerHit ray obj


instance Shape (BoundedNode (SceneObject_ s)) where
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just _  -> innerIntersect ray volume
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfIntersectionTests ray volume
instance SceneObject_ s (BoundedNode (SceneObject_ s)) where
    findHit ray node = case intersect ray $ boundingBox node of
        Nothing -> Nothing
        Just _  -> innerHit ray node

innerIntersect :: Ray Double -> BoundedNode (SceneObject_ s) -> Maybe Intersection
innerIntersect ray (BoundedNode _ innerShape) = intersect ray innerShape
innerIntersect ray (TransformedBoundedNode _ m innerShape) = do
    (t, n, uvw) <- innerIntersect (inverseTransform m ray) innerShape
    return (t, normalTransform m n, uvw)
innerIntersect ray (BoxNode _ innerVolumes) = intersectVts volumesTs
    where
        volumesTs = sortOn fst
                  $ mapMaybe (\innerVolume -> fmap (\(t, _, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerIntersect ray innerVolume of
            Nothing     -> intersectVts vts
            Just (t, n, uvw) -> foldr (closestIntersection . innerIntersect ray . snd) (Just (t, n, uvw)) $ takeWhile ((<t) . fst) vts

innerNumberOfIntersectionTests :: Ray Double -> BoundedNode (SceneObject_ s) -> Int
innerNumberOfIntersectionTests ray (BoundedNode _ innerShape) = numberOfIntersectionTests ray innerShape
innerNumberOfIntersectionTests ray (TransformedBoundedNode _ m innerShape) =
    numberOfIntersectionTests (inverseTransform m ray) innerShape
innerNumberOfIntersectionTests ray (BoxNode _ innerVolumes) = length innerVolumes + nrOfIntersectVts volumesTs
    where
        volumesTs = sortOn fst
                  $ mapMaybe (\innerVolume -> fmap (\(t, _, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        nrOfIntersectVts [] = 0
        nrOfIntersectVts ((_, innerVolume):vts) = innerNumberOfIntersectionTests ray innerVolume + case innerIntersect ray innerVolume of
            Nothing     -> nrOfIntersectVts vts
            Just (t, _, _) -> sum $ map (innerNumberOfIntersectionTests ray . snd) $ takeWhile ((<t) . fst) vts

innerHit :: Ray Double -> BoundedNode (SceneObject_ s) -> Maybe (s, BRDF s, Intersection)
innerHit ray (BoundedNode _ innerObj) = findHit ray innerObj
innerHit ray (TransformedBoundedNode _ m innerObj) = do
    (spec, brdf, (t, n, uvw)) <- innerHit (inverseTransform m ray) innerObj
    return (spec, brdf, (t, normalTransform m n, uvw))
innerHit ray (BoxNode _ innerVolumes) = intersectVts volumesTs
    where
        volumesTs = sortOn fst
                  $ mapMaybe (\innerVolume -> fmap (\(t, _, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerHit ray innerVolume of
            Nothing     -> intersectVts vts
            Just (spec, brdf, (t, n, uvw)) -> foldr (closestHit . innerHit ray . snd) (Just (spec, brdf, (t, n, uvw))) $ takeWhile ((<t) . fst) vts
innerHit ray (BoundedRootNode)
