{-# OPTIONS_GHC -w #-}

module RayTracer.Geometry.Shape
    ( Shape (..)
    , closest
    , Intersection
    , BoundedShapeNode (..)
    , closestIntersection
    , innerIntersect
    , innerNumberOfIntersectionTests
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
class (Boundable a, Show a) => Shape a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray Double -> a -> Maybe Intersection
    numberOfIntersectionTests :: Ray Double -> a -> Int
    numberOfIntersectionTests _ _ = 1
    boundedNode :: a -> BoundedShapeNode
    boundedNode shape = ShapeNode (boundingBox shape) shape


data BoundedShapeNode = ShapeBranchNode AABB [BoundedShapeNode]
                      | forall a. (Shape a, Show a) => ShapeNode AABB a
                      | BoxNode AABB
                      | TransformedShapeNode AABB (Transformation Double) BoundedShapeNode
instance Show BoundedShapeNode where
    show (ShapeBranchNode box branches) = "ShapeBranchNode (" ++ show box ++ ") " ++ show branches
    show (ShapeNode box shape) = "ShapeNode (" ++ show box ++ ") (" ++ show shape ++ ")"
    show (BoxNode box) = "BoxNode (" ++ show box ++ ")"
    show (TransformedShapeNode box tr node) = "TransformedShapeNode (" ++ show box ++ ") (" ++ show tr ++ ") (" ++ show node ++ ")"
instance Boundable BoundedShapeNode where
    boundingBox (ShapeBranchNode box _) = box
    boundingBox (ShapeNode box _) = box
    boundingBox (BoxNode box) = box
    boundingBox (TransformedShapeNode box _ _) = box
instance Shape BoundedShapeNode where
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just intersection -> innerIntersect ray intersection volume
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfIntersectionTests ray volume
    boundedNode = id

innerIntersect :: Ray Double -> Intersection -> BoundedShapeNode -> Maybe Intersection
innerIntersect ray _ (ShapeNode _ innerShape) = intersect ray innerShape
innerIntersect ray _ (TransformedShapeNode _ m innerNode) = do
    (t, n, uvw) <- intersect (inverseTransform m ray) innerNode
    return (t, normalTransform m n, uvw)
innerIntersect ray outerIntersection (BoxNode _) = return outerIntersection
innerIntersect ray _ (ShapeBranchNode _ innerVolumes) = intersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        test vts t = takeWhile ((<t) . getT) vts
        intersectVts [] = Nothing
        intersectVts ((outer, innerVolume):vts) = case innerIntersect ray outer innerVolume of
            Nothing     -> intersectVts vts
            Just intersection@(t, n, uvw) -> foldr (closestIntersection . uncurry (innerIntersect ray)) (Just intersection) $ takeWhile ((<t) . getT) vts

innerNumberOfIntersectionTests :: Ray Double -> BoundedShapeNode -> Int
innerNumberOfIntersectionTests ray (ShapeNode _ innerShape) = numberOfIntersectionTests ray innerShape
innerNumberOfIntersectionTests ray (TransformedShapeNode _ m innerShape) =
    numberOfIntersectionTests (inverseTransform m ray) innerShape
innerNumberOfIntersectionTests ray (BoxNode _) = 0
innerNumberOfIntersectionTests ray (ShapeBranchNode _ innerVolumes) = (sum $ map (numberOfIntersectionTests ray . boundingBox) innerVolumes) + nrOfIntersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        nrOfIntersectVts [] = 0
        nrOfIntersectVts ((outer, innerVolume):vts) = innerNumberOfIntersectionTests ray innerVolume + case innerIntersect ray outer innerVolume of
            Nothing     -> nrOfIntersectVts vts
            Just (t, _, _) -> sum $ map (innerNumberOfIntersectionTests ray . snd) $ takeWhile ((<t) . getT) vts


instance (Shape a) => Shape [a] where
    intersect ray = foldr (closestIntersection . intersect ray) Nothing
    numberOfIntersectionTests ray = sum . map (numberOfIntersectionTests ray)
    boundedNode [] = ShapeBranchNode (boundingBox ([] :: [AABB])) []
    boundedNode shapes = splitBoundingVolumes (boundingBox bvs) bvs
        where
            bvs = map boundedNode shapes

splitBoundingVolumes :: AABB -> [BoundedShapeNode] -> BoundedShapeNode
splitBoundingVolumes _ [bv] = bv
splitBoundingVolumes box bvs
    | areaX <= areaY && areaX <= areaZ = ShapeBranchNode box [splitBoundingVolumes leftBoxX leftX, splitBoundingVolumes rightBoxX rightX]
    | areaY <= areaZ                   = ShapeBranchNode box [splitBoundingVolumes leftBoxY leftY, splitBoundingVolumes rightBoxY rightY]
    | otherwise                        = ShapeBranchNode box [splitBoundingVolumes leftBoxZ leftZ, splitBoundingVolumes rightBoxZ rightZ]
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


instance (Shape a) => Shape (Transformed a) where
    intersect ray (Transformed m s) = do
        (t, n, uvw) <- intersect (inverseTransform m ray) s
        return (t, normalTransform m n, uvw)
    numberOfIntersectionTests ray (Transformed t shape) = numberOfIntersectionTests (inverseTransform t ray) shape
    boundedNode (Transformed tr shape) = TransformedShapeNode box tr innerNode
        where
            innerNode = boundedNode shape
            AABB (Point minx miny minz) (Point maxx maxy maxz) _ = boundingBox innerNode
            box = getAABB $ map (transform tr) [ Point minx miny minz, Point minx miny maxz
                                               , Point minx maxy minz, Point minx maxy maxz
                                               , Point maxx miny minz, Point maxx miny maxz
                                               , Point maxx maxy minz, Point maxx maxy maxz]


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
    boundedNode box = BoxNode box
