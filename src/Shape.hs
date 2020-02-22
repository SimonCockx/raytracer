{-# LANGUAGE ExistentialQuantification #-}

module Shape
    ( ShapeClass (..)
    , Shape (..)
    , Sphere (..)
    ) where

import Vector hiding (map)
import Ray
import Data.Maybe

-- | A class representing a shape that can be intersected by a ray.
class ShapeClass a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray -> a -> Maybe Double

instance (ShapeClass a) => ShapeClass [a] where
    intersect ray shapes = foldr maybeMin Nothing $ map (intersect ray) shapes
        where
            maybeMin Nothing Nothing = Nothing
            maybeMin (Just x) Nothing = Just x
            maybeMin Nothing (Just y) = Just y
            maybeMin (Just x) (Just y) = Just $ min x y

-- | A wrapper to allow a heterogeneous list of shapes.
data Shape = forall a. ShapeClass a => Shape a

instance ShapeClass Shape where
    intersect ray (Shape shape) = intersect ray shape


-- | A type representing a sphere with given center and radius.
data Sphere = 
    -- | Construct a sphere with given center and radius.
    Sphere {
        -- | The center of the sphere
        center :: Point Double,
        -- | The radius of the sphere
        radius :: Double}

instance ShapeClass Sphere where
    intersect ray (Sphere cs r)
        | d < 0  = Nothing
        | d == 0 = let t = -b/2 in if t < 0 then Nothing else Just t
        | otherwise = 
            let dSqrt = sqrt d
                t1 = (-b - dSqrt)/2 in
            if t1 >= 0 then Just t1 
            else let t2 = (-b + dSqrt)/2 in
                if t2 >= 0 then Just t2
                else Nothing
        where
            b = 2 * (l <.> (o <-> cs))
            c = normSqr (o <-> cs) - r*r
            d = b*b - 4*c
            l = direction ray
            o = origin ray

