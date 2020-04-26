module RayTracer.Geometry.Shape
    ( Shape (..)
    , closest
    , Intersection
    , ShapeWrapper (..)
    , DoubleSided (..)
    , BoundedShapeNode (..)
    , closestIntersection
    , innerIntersect
    , innerNumberOfIntersectionTests
    , AABB (..)
    , spaceAABB
    , createAABB
    , getArea
    , getAABB
    , getEnclosingAABB
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Transformation

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
class (Show a) => Shape a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray Double -> a -> Maybe Intersection
    numberOfIntersectionTests :: Ray Double -> a -> Int
    numberOfIntersectionTests _ _ = 1
    boundingBox :: a -> AABB
    boundedNode :: a -> BoundedShapeNode
    boundedNode shape = ShapeNode (boundingBox shape) shape

data ShapeWrapper = forall a. (Shape a) => ShapeWrapper a
instance Show ShapeWrapper where
    show (ShapeWrapper shape) = "ShapeWrapper (" ++ show shape ++ ")"
instance Shape ShapeWrapper where
    intersect ray (ShapeWrapper shape) = intersect ray shape
    numberOfIntersectionTests ray (ShapeWrapper shape) = numberOfIntersectionTests ray shape
    boundingBox (ShapeWrapper shape) = boundingBox shape
    boundedNode (ShapeWrapper shape) = boundedNode shape

instance Shape () where
    intersect _ () = Nothing
    numberOfIntersectionTests _ () = 0
    boundingBox () = createAABB (pure 1) (pure (-1))
    boundedNode = UnboundedNode

newtype DoubleSided a = DoubleSided a
    deriving (Show)
instance (Shape a) => Shape (DoubleSided a) where
    intersect ray (DoubleSided shape) = do
        (t, n, uvw) <- intersect ray shape
        return $ (t, if direction ray <.> n > 0 then negateV n else n, uvw)
    numberOfIntersectionTests ray (DoubleSided shape) = numberOfIntersectionTests ray shape
    boundingBox (DoubleSided shape) = boundingBox shape
    boundedNode (DoubleSided shape) = boundedNode shape
instance (Transformable a b) => Transformable (DoubleSided a) b where
    transform tr (DoubleSided shape) = DoubleSided (transform tr shape)

data BoundedShapeNode = ShapeBranchNode AABB [BoundedShapeNode]
                      | forall a. (Shape a, Show a) => ShapeNode AABB a
                      | forall a. (Shape a, Show a) => UnboundedNode a
                      | TransformedShapeNode AABB (Transformation Double) BoundedShapeNode
instance Show BoundedShapeNode where
    show (ShapeBranchNode box branches) = "ShapeBranchNode (" ++ show box ++ ") " ++ show branches
    show (ShapeNode box shape) = "ShapeNode (" ++ show box ++ ") (" ++ show shape ++ ")"
    show (UnboundedNode shape) = "UnboundedNode (" ++ show shape ++ ")"
    show (TransformedShapeNode box tr node) = "TransformedShapeNode (" ++ show box ++ ") (" ++ show tr ++ ") (" ++ show node ++ ")"
instance Shape BoundedShapeNode where
    intersect ray (UnboundedNode shape) = intersect ray shape
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just _  -> innerIntersect ray volume
    numberOfIntersectionTests ray (UnboundedNode shape) = numberOfIntersectionTests ray shape
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfIntersectionTests ray volume
    boundingBox (ShapeBranchNode box _) = box
    boundingBox (ShapeNode box _) = box
    boundingBox (UnboundedNode _) = spaceAABB
    boundingBox (TransformedShapeNode box _ _) = box
    boundedNode = id

innerIntersect :: Ray Double -> BoundedShapeNode -> Maybe Intersection
innerIntersect ray (ShapeNode _ innerShape) = intersect ray innerShape
innerIntersect ray (TransformedShapeNode _ m innerNode) = do
    (t, n, uvw) <- intersect (inverseTransform m ray) innerNode
    return (t, normalTransform m n, uvw)
innerIntersect ray (UnboundedNode shape) = intersect ray shape
innerIntersect ray (ShapeBranchNode _ innerVolumes) = intersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerIntersect ray innerVolume of
            Nothing     -> intersectVts vts
            Just intersection@(t, _, _) -> foldr (\vt acc -> closestIntersection acc $ innerIntersect ray $ snd vt) (Just intersection) $ takeWhile ((<t) . getT) vts

innerNumberOfIntersectionTests :: Ray Double -> BoundedShapeNode -> Int
innerNumberOfIntersectionTests ray (ShapeNode _ innerShape) = numberOfIntersectionTests ray innerShape
innerNumberOfIntersectionTests ray (TransformedShapeNode _ m innerShape) =
    numberOfIntersectionTests (inverseTransform m ray) innerShape
innerNumberOfIntersectionTests ray (UnboundedNode shape) = numberOfIntersectionTests ray shape
innerNumberOfIntersectionTests ray (ShapeBranchNode _ innerVolumes) = (sum $ map (numberOfIntersectionTests ray . boundingBox) innerVolumes) + nrOfIntersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        nrOfIntersectVts [] = 0
        nrOfIntersectVts ((_, innerVolume):vts) = innerNumberOfIntersectionTests ray innerVolume + case innerIntersect ray innerVolume of
            Nothing     -> nrOfIntersectVts vts
            Just (t, _, _) -> sum $ map (innerNumberOfIntersectionTests ray . snd) $ takeWhile ((<t) . getT) vts


instance (Shape a) => Shape [a] where
    intersect ray = foldr (closestIntersection . intersect ray) Nothing
    numberOfIntersectionTests ray = sum . map (numberOfIntersectionTests ray)
    boundingBox boundables = getEnclosingAABB $ map boundingBox boundables
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
    boundingBox (Transformed t s) = getAABB points
        where
            AABB (Point minx miny minz) (Point maxx maxy maxz) _ = boundingBox s
            points = map (transform t) [ Point minx miny minz, Point minx miny maxz
                                       , Point minx maxy minz, Point minx maxy maxz
                                       , Point maxx miny minz, Point maxx miny maxz
                                       , Point maxx maxy minz, Point maxx maxy maxz ]
    boundedNode ts@(Transformed tr shape) = TransformedShapeNode (boundingBox ts) tr innerNode
        where
            innerNode = boundedNode shape


data AABB = AABB {minimumPoint :: Point Double, maximumPoint :: Point Double, centroid :: Point Double}
    deriving (Show)
spaceAABB :: AABB
spaceAABB = AABB (pure (-1/0)) (pure (1/0)) (pure 0)
createAABB :: Point Double -> Point Double -> AABB
createAABB minP maxP = AABB minP maxP $ minP <+^ (maxP <-> minP)^/2
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
    boundingBox = id
    boundedNode = UnboundedNode
