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
    , Transformed (..)
    , BoundedNode (..)
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
getAABB points = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) points
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) points
getEnclosingAABB :: [AABB] -> AABB
getEnclosingAABB aabbs = createAABB minPoint maxPoint
    where
        minPoint = foldr1 (\(Point x y z) (Point minx miny minz) -> Point (min minx x) (min miny y) (min minz z)) $ map minimumPoint aabbs
        maxPoint = foldr1 (\(Point x y z) (Point maxx maxy maxz) -> Point (max maxx x) (max maxy y) (max maxz z)) $ map maximumPoint aabbs


data Transformed a = Transformed (Transformation Double) a
    deriving (Show)
instance Transformable (Transformed a) Double where
    transform t (Transformed t' s) = Transformed (t `transform` t') s
instance (Boundable a) => Boundable (Transformed a) where
    type BoundedContent (Transformed a) = BoundedContent a
    boundingBox (Transformed t s) = getAABB points
        where
            AABB (Point minx miny minz) (Point maxx maxy maxz) _ = boundingBox s
            points = map (transform t) [ Point minx miny minz, Point minx miny maxz
                                       , Point minx maxy minz, Point minx maxy maxz
                                       , Point maxx miny minz, Point maxx miny maxz
                                       , Point maxx maxy minz, Point maxx maxy maxz ]
    boundingVolume ts@(Transformed t s) = TransformedBoundedNode (boundingBox ts) t $ boundingVolume s


data BoundedNode (c :: * -> Constraint) = BoxNode AABB [BoundedNode c]
                                        | forall a. (c a, Show a) => BoundedNode AABB a
                                        | TransformedBoundedNode AABB (Transformation Double) (BoundedNode c)
                                        | forall a d. (c a, Show a) => SubtreeNode a (BoundedNode d)
instance Show (BoundedNode c) where
    show (BoxNode box innerNodes) = "BoxNode (" ++ show box ++ ") " ++ show innerNodes
    show (BoundedNode box contents) = "BoundedNode (" ++ show box ++ ") (" ++ show contents ++ ")"
    show (TransformedBoundedNode box t node) = "TransformedBoundedNode (" ++ show box ++ ") (" ++ show t ++ ") (" ++ show node ++ ")"
    show (SubtreeNode contents subtree) = "SubtreeNode (" ++ show contents ++ ") (" ++ show subtree ++ ")" 


class (Show a) => Boundable a where
    type BoundedContent a :: * -> Constraint
    boundingBox :: a -> AABB
    boundingVolume :: a -> BoundedNode (BoundedContent a)
    default boundingVolume :: (BoundedContent a a) => a -> BoundedNode (BoundedContent a)
    boundingVolume contents = BoundedNode (boundingBox contents) contents

instance Boundable (BoundedNode c) where
    type BoundedContent (BoundedNode c) = c
    boundingBox (BoxNode box _) = box
    boundingBox (BoundedNode box _) = box
    boundingBox (TransformedBoundedNode box _ _) = box
    boundingBox (SubtreeNode _ subtree) = boundingBox subtree
    boundingVolume = id

instance (Boundable a) => Boundable [a] where
    type BoundedContent [a] = BoundedContent a
    boundingBox boundables = getEnclosingAABB $ map boundingBox boundables
    boundingVolume shapes = splitBoundingVolumes (boundingBox bvs) bvs
        where
            bvs = map boundingVolume shapes

splitBoundingVolumes :: AABB -> [BoundedNode c] -> BoundedNode c
splitBoundingVolumes _ [bv] = bv
splitBoundingVolumes box bvs
    | areaX <= areaY && areaX <= areaZ = BoxNode box [splitBoundingVolumes leftBoxX leftX, splitBoundingVolumes rightBoxX rightX]
    | areaY <= areaZ                   = BoxNode box [splitBoundingVolumes leftBoxY leftY, splitBoundingVolumes rightBoxY rightY]
    | otherwise                        = BoxNode box [splitBoundingVolumes leftBoxZ leftZ, splitBoundingVolumes rightBoxZ rightZ]
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