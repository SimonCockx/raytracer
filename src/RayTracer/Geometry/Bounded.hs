{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -w #-}

module RayTracer.Geometry.Bounded
    ( AABB (minimumPoint, maximumPoint, centroid)
    , pattern AABB
    , createAABB
    , getArea
    , getAABB
    , getEnclosingAABB
    , Boundable (..)
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Transformation

import GHC.Exts (Constraint)
import Data.List hiding (intersect)

pattern AABB :: Point Double -> Point Double -> Point Double -> AABB
pattern AABB minp maxp cent <- AABBox minp maxp cent

data AABB = AABBox {minimumPoint :: Point Double, maximumPoint :: Point Double, centroid :: Point Double}
    deriving (Show)
createAABB :: Point Double -> Point Double -> AABB
createAABB minP maxP = AABBox minP maxP $ minP <+^ (maxP <-> minP)^/2
getArea :: AABB -> Double
getArea (AABB minP maxP _) = 2*(dx*dy + dy*dz + dz*dx)
    where
        Vector dx dy dz = minP <-> maxP
getAABB :: [Point Double] -> AABB
getAABB [] = createAABB (pure 1) (pure (-1))
getAABB points = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) points
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) points
getEnclosingAABB :: [AABB] -> AABB
getEnclosingAABB [] = createAABB (pure 1) (pure (-1))
getEnclosingAABB aabbs = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) $ map minimumPoint aabbs
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) $ map maximumPoint aabbs



class Boundable a where
    boundingBox :: a -> AABB

instance Boundable AABB where
    boundingBox = id

instance (Boundable a) => Boundable [a] where
    boundingBox boundables = getEnclosingAABB $ map boundingBox boundables
--     boundingVolume shapes = splitBoundingVolumes (boundingBox bvs) bvs
--         where
--             bvs = map boundingVolume shapes

-- splitBoundingVolumes :: AABB -> [BoundedNode c] -> BoundedNode c
-- splitBoundingVolumes _ [bv] = bv
-- splitBoundingVolumes box bvs
--     | areaX <= areaY && areaX <= areaZ = BoxNode box [splitBoundingVolumes leftBoxX leftX, splitBoundingVolumes rightBoxX rightX]
--     | areaY <= areaZ                   = BoxNode box [splitBoundingVolumes leftBoxY leftY, splitBoundingVolumes rightBoxY rightY]
--     | otherwise                        = BoxNode box [splitBoundingVolumes leftBoxZ leftZ, splitBoundingVolumes rightBoxZ rightZ]
--     where
--         middle = (length bvs + 1) `div` 2
--         getGroups projection = splitAt middle $ sortOn (\bv -> projection $ centroid $ boundingBox $ bv) bvs
--         (leftX, rightX) = getGroups (\(Point x _ _) -> x)
--         (leftBoxX, rightBoxX) = (boundingBox leftX, boundingBox rightX)
--         areaX = getArea leftBoxX + getArea rightBoxX
--         (leftY, rightY) = getGroups (\(Point _ y _) -> y)
--         (leftBoxY, rightBoxY) = (boundingBox leftY, boundingBox rightY)
--         areaY = getArea leftBoxY + getArea rightBoxY
--         (leftZ, rightZ) = getGroups (\(Point _ _ z) -> z)
--         (leftBoxZ, rightBoxZ) = (boundingBox leftZ, boundingBox rightZ)
--         areaZ = getArea leftBoxZ + getArea rightBoxZ


instance (Boundable a) => Boundable (Transformed a) where
    boundingBox (Transformed t s) = getAABB points
        where
            AABB (Point minx miny minz) (Point maxx maxy maxz) _ = boundingBox s
            points = map (transform t) [ Point minx miny minz, Point minx miny maxz
                                       , Point minx maxy minz, Point minx maxy maxz
                                       , Point maxx miny minz, Point maxx miny maxz
                                       , Point maxx maxy minz, Point maxx maxy maxz ]
