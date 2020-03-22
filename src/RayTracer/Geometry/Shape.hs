{-# OPTIONS_GHC -w #-}

module RayTracer.Geometry.Shape
    ( Shape (..)
    , closest
    , Intersection
    , closestIntersection
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Transformation
import RayTracer.Geometry.Bounded

import Data.Maybe
import Data.List hiding (intersect)


type Intersection = (Double, Vector Double, Vector Double)

closest :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a
closest f = maybeC (\x y -> if f x < f y then x else y)
    where
        maybeC :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
        maybeC _ Nothing Nothing = Nothing
        maybeC _ (Just x) Nothing = Just x
        maybeC _ Nothing (Just y) = Just y
        maybeC combine (Just x) (Just y) = Just $ combine x y
{-# INLINE closest #-}

closestIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
closestIntersection = closest $ \(t, _, _) -> t
{-# INLINE closestIntersection #-}

-- | A class representing a shape that can be intersected by a ray.
class Shape a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray Double -> a -> Maybe Intersection
    numberOfIntersectionTests :: Ray Double -> a -> Int
    numberOfIntersectionTests _ _ = 1


instance Shape (BoundedNode Shape) where
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just _  -> innerIntersect ray volume
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfIntersectionTests ray volume

innerIntersect :: Ray Double -> BoundedNode Shape -> Maybe Intersection
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

innerNumberOfIntersectionTests :: Ray Double -> BoundedNode Shape -> Int
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


instance (Shape a) => Shape [a] where
    intersect ray = foldr (closestIntersection . intersect ray) Nothing
    numberOfIntersectionTests ray = sum . map (numberOfIntersectionTests ray)


instance (Shape a) => Shape (Transformed a) where
    intersect ray (Transformed m s) = do
        (t, n, uvw) <- intersect (inverseTransform m ray) s
        return (t, normalTransform m n, uvw)
    numberOfIntersectionTests ray (Transformed t shape) = numberOfIntersectionTests (inverseTransform t ray) shape


instance Boundable AABB where
    type BoundedContent AABB = Shape
    boundingBox = id
instance Shape AABB where
    intersect ray (AABB minp@(Point minx miny minz) maxp@(Point maxx maxy maxz) c)
        | minx < xo && xo < maxx && miny < yo && yo < maxy && minz < zo && zo < maxz = Just (0, Vector 0 0 0, (/) `fmap` (origin ray <-> c) <*> dp)
        | tmax < 0 || tmin > tmax = Nothing
        | otherwise               = Just (tmin, normal, uvw)
        where
            normal
                | tmin == tx0 = Vector 1 0 0
                | tmin == tx1 = Vector (-1) 0 0
                | tmin == ty0 = Vector 0 1 0
                | tmin == ty1 = Vector 0 (-1) 0
                | tmin == tz0 = Vector 0 0 1
                | otherwise = Vector 0 0 (-1)
            tx0 = (maxx-xo)/xd
            tx1 = (minx-xo)/xd
            ty0 = (maxy-yo)/yd
            ty1 = (miny-yo)/yd
            tz0 = (maxz-zo)/zd
            tz1 = (minz-zo)/zd
            tmin = max (max (min tx0 tx1) (min ty0 ty1)) (min tz0 tz1)
            tmax = min (min (max tx0 tx1) (max ty0 ty1)) (max tz0 tz1)
            dp = maxp <-> minp
            uvw = (/) `fmap` (follow ray tmin <-> c) <*> dp
            Point xo yo zo = origin ray
            Vector xd yd zd = direction ray
