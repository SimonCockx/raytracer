{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Geometry.Shape
    ( Shape (..)
    , closest
    , Intersection
    , closestIntersection
    , BoundingVolume (..)
    , TransformedShape (..)
    , AABB (minimumPoint, maximumPoint, centroid)
    , createAABB
    , getAABB
    , getEnclosingAABB
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Transformation

import Data.Maybe
import Data.List hiding (intersect)


type Intersection = (Double, Vector Double)

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
closestIntersection = closest fst
{-# INLINE closestIntersection #-}

-- | A class representing a shape that can be intersected by a ray.
class (Show a) => Shape a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray Double -> a -> Maybe Intersection
    boundingBox :: a -> AABB
    boundingVolume :: a -> BoundingVolume
    boundingVolume shape = Bounded (boundingBox shape) shape
    numberOfIntersectionTests :: Ray Double -> a -> Int
    numberOfIntersectionTests _ _ = 1


data BoundingVolume = BoundingVolume AABB [BoundingVolume]
                    | forall a. (Shape a, Show a) => Bounded AABB a
                    | TransformedBoundingVolume AABB (Transformation Double) BoundingVolume
instance Show BoundingVolume where
    show (BoundingVolume box innerVolumes) = "BoundingVolume (" ++ show box ++ ") (" ++ show innerVolumes ++ ")"
    show (Bounded box shape) = "Bounded (" ++ show box ++ ") (" ++ show shape ++ ")"
    show (TransformedBoundingVolume box t innerVolume) = "TransformedBoundingVolume (" ++ show box ++ ") (" ++ show t ++ ") (" ++ show innerVolume ++ ")"
instance Shape BoundingVolume where
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just _  -> innerIntersect ray volume
    boundingBox (Bounded box _) = box
    boundingBox (BoundingVolume box _) = box
    boundingBox (TransformedBoundingVolume box _ _) = box
    boundingVolume = id
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfIntersectionTests ray volume

innerIntersect :: Ray Double -> BoundingVolume -> Maybe Intersection
innerIntersect ray (Bounded _ innerShape) = intersect ray innerShape
innerIntersect ray (TransformedBoundingVolume _ m innerShape) = do
    (t, n) <- innerIntersect (inverseTransform m ray) innerShape
    return (t, normalTransform m n)
innerIntersect ray (BoundingVolume _ innerVolumes) = intersectVts volumesTs
    where
        volumesTs = sortOn fst 
                  $ mapMaybe (\innerVolume -> fmap (\(t, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerIntersect ray innerVolume of
            Nothing     -> intersectVts vts
            Just (t, n) -> foldr (closestIntersection . innerIntersect ray . snd) (Just (t, n)) $ takeWhile ((<t) . fst) vts

innerNumberOfIntersectionTests :: Ray Double -> BoundingVolume -> Int
innerNumberOfIntersectionTests ray (Bounded _ innerShape) = numberOfIntersectionTests ray innerShape
innerNumberOfIntersectionTests ray (TransformedBoundingVolume _ m innerShape) = 
    numberOfIntersectionTests (inverseTransform m ray) innerShape
innerNumberOfIntersectionTests ray (BoundingVolume _ innerVolumes) = length innerVolumes + nrOfIntersectVts volumesTs
    where
        volumesTs = sortOn fst 
                  $ mapMaybe (\innerVolume -> fmap (\(t, _) -> (t, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        
        nrOfIntersectVts [] = 0
        nrOfIntersectVts ((_, innerVolume):vts) = innerNumberOfIntersectionTests ray innerVolume + case innerIntersect ray innerVolume of
            Nothing     -> nrOfIntersectVts vts
            Just (t, _) -> sum $ map (innerNumberOfIntersectionTests ray . snd) $ takeWhile ((<t) . fst) vts


instance (Shape a) => Shape [a] where
    intersect ray = foldr (closestIntersection . intersect ray) Nothing
    boundingBox shapes = getEnclosingAABB $ map boundingBox shapes
    boundingVolume shapes = splitBoundingVolumes (boundingBox bvs) bvs
        where
            bvs = map boundingVolume shapes
    numberOfIntersectionTests ray = sum . map (numberOfIntersectionTests ray)

splitBoundingVolumes :: AABB -> [BoundingVolume] -> BoundingVolume
splitBoundingVolumes _ [bv] = bv
splitBoundingVolumes box bvs
    | areaX <= areaY && areaX <= areaZ = BoundingVolume box [splitBoundingVolumes leftBoxX leftX, splitBoundingVolumes rightBoxX rightX]
    | areaY <= areaZ                   = BoundingVolume box [splitBoundingVolumes leftBoxY leftY, splitBoundingVolumes rightBoxY rightY]
    | otherwise                        = BoundingVolume box [splitBoundingVolumes leftBoxZ leftZ, splitBoundingVolumes rightBoxZ rightZ]
    where
        middle = (length bvs + 1) `div` 2
        getGroups projection = splitAt middle $ sortOn (\bv -> projection $ centroid $ boundingBox $ bv) bvs
        (leftX, rightX) = getGroups (\(Point x _ _) -> x)
        (leftBoxX, rightBoxX) = (boundingBox leftX, boundingBox rightX)
        areaX = getArea leftBoxX + getArea rightBoxX
        (leftY, rightY) = getGroups (\(Point _ y _) -> y)
        (leftBoxY, rightBoxY) = (boundingBox leftY, boundingBox rightY)
        areaY = getArea leftBoxY + getArea rightBoxY
        (leftZ, rightZ) = getGroups (\(Point _ _ z) -> z)
        (leftBoxZ, rightBoxZ) = (boundingBox leftZ, boundingBox rightZ)
        areaZ = getArea leftBoxZ + getArea rightBoxZ


data TransformedShape a = Transformed (Transformation Double) a
instance (Shape a) => Transformable (TransformedShape a) Double where
    transform t (Transformed t' s) = Transformed (t `transform` t') s

instance (Show a) => Show (TransformedShape a) where
    show (Transformed t s) = "Transformed (" ++ show t ++ ") (" ++ show s ++ ")"

instance (Shape a) => Shape (TransformedShape a) where
    intersect ray (Transformed m s) = do
        (t, n) <- intersect (inverseTransform m ray) s
        return (t, normalTransform m n)
    boundingBox (Transformed t s) = t `transform` boundingBox s
    boundingVolume ts@(Transformed t s) = TransformedBoundingVolume (boundingBox ts) t $ boundingVolume s
    numberOfIntersectionTests ray (Transformed t shape) = numberOfIntersectionTests (inverseTransform t ray) shape




data AABB = AABB {minimumPoint :: Point Double, maximumPoint :: Point Double, centroid :: Point Double}
    deriving (Show)
createAABB :: Point Double -> Point Double -> AABB
createAABB minP maxP = AABB minP maxP $ minP <+^ (maxP <-> minP)^/2
getArea :: AABB -> Double
getArea (AABB minP maxP _) = 2*(dx*dy + dy*dz + dz*dx)
    where
        Vector dx dy dz = minP <-> maxP
getAABB :: [Point Double] -> AABB
getAABB points = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) points
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) points
getEnclosingAABB :: [AABB] -> AABB
getEnclosingAABB aabbs = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) $ map minimumPoint aabbs
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) $ map maximumPoint aabbs
instance Shape AABB where
    intersect ray (AABB (Point minx miny minz) (Point maxx maxy maxz) _)
        | tmax < 0 || tmin > tmax = Nothing
        | otherwise               = Just (tmin, normal)
        where
            tx0 = (maxx-xo)/xd
            tx1 = (minx-xo)/xd
            ty0 = (maxy-yo)/yd
            ty1 = (miny-yo)/yd
            tz0 = (maxz-zo)/zd
            tz1 = (minz-zo)/zd
            tmin = max (max (min tx0 tx1) (min ty0 ty1)) (min tz0 tz1)
            normal = if tmin == tx0 then Vector 1 0 0
                else if tmin == tx1 then Vector (-1) 0 0
                else if tmin == ty0 then Vector 0 1 0
                else if tmin == ty1 then Vector 0 (-1) 0
                else if tmin == tz0 then Vector 0 0 1
                else Vector 0 0 (-1)
            tmax = min (min (max tx0 tx1) (max ty0 ty1)) (max tz0 tz1)
            Point xo yo zo = origin ray
            Vector xd yd zd = direction ray
    boundingBox = id
instance Transformable AABB Double where
    transform t (AABB (Point minx miny minz) (Point maxx maxy maxz) _) = getAABB points
        where
            points = map (transform t) [ Point minx miny minz, Point minx miny maxz
                                       , Point minx maxy minz, Point minx maxy maxz
                                       , Point maxx miny minz, Point maxx miny maxz
                                       , Point maxx maxy minz, Point maxx maxy maxz ]
