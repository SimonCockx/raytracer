{-# OPTIONS_GHC -w #-}

module RayTracer.Core.SceneObject
    ( Object (..)
    , findInspectingHit
    , findReflectingHit
    , findShadowHit
    , SceneObject (..)
    , BoundedObjectNode (..)
    , Surface (..)
    , simpleObject
    , withMaterial
    ) where

import RayTracer.Geometry
import RayTracer.Lightning

import RayTracer.Core.Sampling

import Data.Maybe
import Data.List hiding (intersect)
import Data.Foldable

import RayTracer.Random


class (Shape a) => Object a s where
    findHit :: Ray Double -> a -> Maybe (MaterialHit s)
    default findHit :: (Material a s, Spectrum s) => Ray Double -> a -> Maybe (MaterialHit s)
    findHit ray object = do
        intersection <- intersect ray object
        return $ hitMaterial object ray intersection
    boundedObjectNode :: a -> BoundedObjectNode s
    default boundedObjectNode :: (Material a s) => a -> BoundedObjectNode s
    boundedObjectNode object = MaterialNode object $ boundedNode object

findInspectingHit :: (Object a s) => Ray Double -> a -> Maybe (InspectingHit s)
findInspectingHit ray obj = inspectHit <$> findHit ray obj

findReflectingHit :: (Object a s, MonadRandom m) => Ray Double -> a -> Maybe (m (ReflectingHit s))
findReflectingHit ray obj = reflectHit <$> findHit ray obj

findShadowHit :: (Object a s, MonadRandom m) => Ray Double -> a -> SamplingStrategy -> [Light s] -> Maybe (m (ShadowHit s))
findShadowHit ray obj strat lights = do
    matHit <- findHit ray obj
    return $ shadowReflectHit matHit strat lights

instance (Show s, Spectrum s) => Object () s where
    findHit _ () = Nothing
    boundedObjectNode () = MaterialNode blackMaterial $ boundedNode ()



data SceneObject s = forall a. (Object a s) => SceneObject a
                   | forall l. (Shape l, LightSource l s) => SceneLight l

instance Show (SceneObject s) where
    show (SceneObject obj) = "SceneObject (" ++ show obj ++ ")"
    show (SceneLight obj) = "SceneLight (" ++ show obj ++ ")"
instance Shape (SceneObject s) where
    intersect ray (SceneObject obj) = intersect ray obj
    intersect ray (SceneLight obj) = intersect ray obj
    numberOfIntersectionTests ray (SceneObject obj) = numberOfIntersectionTests ray obj
    numberOfIntersectionTests ray (SceneLight obj) = numberOfIntersectionTests ray obj
    boundingBox (SceneObject obj) = boundingBox obj
    boundingBox (SceneLight obj) = boundingBox obj
    boundedNode (SceneObject obj) = boundedNode obj
    boundedNode (SceneLight obj) = boundedNode obj
instance (Spectrum s1, s1 ~ s2, Show s1) => Object (SceneObject s1) s2 where
    findHit ray (SceneObject obj) = findHit ray obj
    findHit ray (SceneLight light) = do
        intersection <- intersect ray light
        return $ hitMaterial (Light light) ray intersection
    boundedObjectNode (SceneObject obj) = boundedObjectNode obj
    boundedObjectNode (SceneLight light) = MaterialNode (Light light) $ boundedNode light


closestHit :: Maybe (MaterialHit s) -> Maybe (MaterialHit s) -> Maybe (MaterialHit s)
closestHit = closest (\matHit -> let InspectingHit _ _ (t, _, _) = inspectHit matHit in t)
instance (Object obj s) => Object [obj] s where
    findHit ray = foldr (closestHit . findHit ray) Nothing
    boundedObjectNode [] = ObjectBranchNode (boundingBox ([] :: [AABB])) []
    boundedObjectNode objects = splitBoundingObjects (boundingBox bvs) bvs
        where
            bvs = map boundedObjectNode objects

splitBoundingObjects :: AABB -> [BoundedObjectNode s] -> BoundedObjectNode s
splitBoundingObjects _ [bv] = bv
splitBoundingObjects box bvs
    | areaX <= areaY && areaX <= areaZ = ObjectBranchNode box [splitBoundingObjects leftBoxX leftX, splitBoundingObjects rightBoxX rightX]
    | areaY <= areaZ                   = ObjectBranchNode box [splitBoundingObjects leftBoxY leftY, splitBoundingObjects rightBoxY rightY]
    | otherwise                        = ObjectBranchNode box [splitBoundingObjects leftBoxZ leftZ, splitBoundingObjects rightBoxZ rightZ]
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


data Surface a s = forall m. (Material m s, Show m) => Surface a m

instance (Show a) => Show (Surface a s) where
    show (Surface shape material) = "Surface (" ++ show shape ++ ") (" ++ show material ++ ")"
instance (Shape a) => Shape (Surface a s) where
    intersect ray (Surface shape _) = intersect ray shape
    numberOfIntersectionTests ray (Surface shape _) = numberOfIntersectionTests ray shape
    boundingBox (Surface shape _) = boundingBox shape
    boundedNode (Surface shape _) = boundedNode shape
instance (s1 ~ s2) => Material (Surface a s1) s2 where
    inspect (Surface _ material) = inspect material
    reflect (Surface _ material) = reflect material
    shadowReflect (Surface _ material) = shadowReflect material
instance (Shape a, s1 ~ s2, Spectrum s1) => Object (Surface a s1) s2 where
instance (Transformable a b) => Transformable (Surface a s) b where
    transform t (Surface shape brdf) = Surface (t `transform` shape) brdf


simpleObject :: (Shape a, Show s, Spectrum s) => a -> SceneObject s
simpleObject = (`withMaterial` whiteMaterial)

withMaterial :: (Material m s, Show m, Shape a, Spectrum s) => a -> m -> SceneObject s
withMaterial shape = SceneObject . Surface shape


instance (Object a s) => Object (Transformed a) s where
    findHit ray (Transformed tr obj) = do
        matHit <- findHit (inverseTransform tr ray) obj
        return $ transform tr matHit
    boundedObjectNode (Transformed tr obj) = TransformedObjectNode box tr innerNode
        where
            innerNode = boundedObjectNode obj
            AABB (Point minx miny minz) (Point maxx maxy maxz) _ = boundingBox innerNode
            box = getAABB $ map (transform tr) [ Point minx miny minz, Point minx miny maxz
                                               , Point minx maxy minz, Point minx maxy maxz
                                               , Point maxx miny minz, Point maxx miny maxz
                                               , Point maxx maxy minz, Point maxx maxy maxz]

data BoundedObjectNode s = ObjectBranchNode AABB [BoundedObjectNode s]
                         | forall m. (Material m s, Show m) => MaterialNode m BoundedShapeNode
                         | TransformedObjectNode AABB (Transformation Double) (BoundedObjectNode s)
instance Show (BoundedObjectNode s) where
    show (ObjectBranchNode box branches) = "ObjectBranchNode (" ++ show box ++ ") " ++ show branches
    show (MaterialNode obj shapeNode) = "MaterialNode (" ++ show obj ++ ") (" ++ show shapeNode ++ ")"
    show (TransformedObjectNode box tr node) = "TransformedObjectNode (" ++ show box ++ ") (" ++ show tr ++ ") (" ++ show node ++ ")"
instance Shape (BoundedObjectNode s) where
    intersect ray volume = case intersect ray $ boundingBox volume of
        Nothing -> Nothing
        Just _  -> innerObjectIntersect ray volume
    numberOfIntersectionTests ray volume = numberOfIntersectionTests ray (boundingBox volume) + case intersect ray $ boundingBox volume of
        Nothing -> 0
        Just _  -> innerNumberOfObjectIntersectionTests ray volume
    boundingBox (ObjectBranchNode box _) = box
    boundingBox (MaterialNode _ shapeNode) = boundingBox shapeNode
    boundingBox (TransformedObjectNode box _ _) = box
    boundedNode (ObjectBranchNode box innerNodes) = ShapeBranchNode box $ map boundedNode innerNodes
    boundedNode (MaterialNode _ shapeNode) = shapeNode
    boundedNode (TransformedObjectNode box tr innerNode) = TransformedShapeNode box tr $ boundedNode innerNode
instance (Spectrum s1, s1 ~ s2) => Object (BoundedObjectNode s1) s2 where
    findHit ray node = case intersect ray $ boundingBox node of
        Nothing -> Nothing
        Just _  -> innerHit ray node
    boundedObjectNode = id


innerObjectIntersect :: Ray Double -> BoundedObjectNode s -> Maybe Intersection
innerObjectIntersect ray (MaterialNode _ innerShape) = innerIntersect ray innerShape
innerObjectIntersect ray (TransformedObjectNode _ m innerNode) = do
    (t, n, uvw) <- intersect (inverseTransform m ray) innerNode
    return (t, normalTransform m n, uvw)
innerObjectIntersect ray (ObjectBranchNode _ innerVolumes) = intersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        test vts t = takeWhile ((<t) . getT) vts
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerObjectIntersect ray innerVolume of
            Nothing     -> intersectVts vts
            Just intersection@(t, n, uvw) -> foldr (\vt acc -> closestIntersection acc $ innerObjectIntersect ray $ snd vt) (Just intersection) $ takeWhile ((<t) . getT) vts

innerNumberOfObjectIntersectionTests :: Ray Double -> BoundedObjectNode s -> Int
innerNumberOfObjectIntersectionTests ray (MaterialNode _ innerShape) = innerNumberOfIntersectionTests ray innerShape
innerNumberOfObjectIntersectionTests ray (TransformedObjectNode _ m innerShape) =
    numberOfIntersectionTests (inverseTransform m ray) innerShape
innerNumberOfObjectIntersectionTests ray (ObjectBranchNode _ innerVolumes) = (sum $ map (numberOfIntersectionTests ray . boundingBox) innerVolumes) + nrOfIntersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (, innerVolume) $ intersect ray $ boundingBox innerVolume) innerVolumes
        nrOfIntersectVts [] = 0
        nrOfIntersectVts ((_, innerVolume):vts) = innerNumberOfObjectIntersectionTests ray innerVolume + case innerObjectIntersect ray innerVolume of
            Nothing     -> nrOfIntersectVts vts
            Just (t, _, _) -> sum $ map (innerNumberOfObjectIntersectionTests ray . snd) $ takeWhile ((<t) . getT) vts


innerHit :: (Spectrum s) => Ray Double -> BoundedObjectNode s -> Maybe (MaterialHit s)
innerHit ray (MaterialNode material shapeNode) = do
    intersection <- innerIntersect ray shapeNode
    return $ hitMaterial material ray intersection
innerHit ray (TransformedObjectNode _ tr innerObj) = do
    matHit <- findHit (inverseTransform tr ray) innerObj
    return $ transform tr matHit
innerHit ray (ObjectBranchNode _ innerVolumes) = intersectVts volumesTs
    where
        getT ((t, _, _), _) = t
        volumesTs = sortOn getT
                  $ mapMaybe (\innerVolume -> fmap (\outer@(t, _, _) -> (outer, innerVolume)) $ intersect ray $ boundingBox innerVolume) innerVolumes
        intersectVts [] = Nothing
        intersectVts ((_, innerVolume):vts) = case innerHit ray innerVolume of
            Nothing     -> intersectVts vts
            Just matHit -> let InspectingHit _ _ (t, _, _) = inspectHit matHit in
                foldr (\vt acc -> closestHit acc $ innerHit ray $ snd vt) (Just matHit) $ takeWhile ((<t) . getT) vts
