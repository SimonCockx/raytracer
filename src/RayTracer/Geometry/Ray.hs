module RayTracer.Geometry.Ray 
    ( Ray (..)
    , createRay
    , follow
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Transformation

-- | A type representing a ray.
data Ray a
    -- | Construct a ray with given origin and direction.
    = Ray {
        -- | Return the origin of the ray.
        origin :: Point a,
        -- | Return the normalized direction of the ray.
        direction :: Vector a}
    deriving (Show)

instance (Floating a, Eq a) => Transformable (Ray a) a where
    transform t ray = Ray p d
        where
            p = transform t $ origin ray
            d = transform t $ direction ray

-- | Creates a new ray with given origin and direction.
--   The direction is normalized in the process.
createRay :: (Floating a, Eq a, AdditiveGroup a)
          => Point a  -- ^ The origin
          -> Vector a -- ^ The direction
          -> Ray a    -- ^ The resulting ray
createRay orig direc = Ray orig (normalize direc)

-- | Return the point that the ray would reach when following the direction of the ray for a specified amount.
follow :: (Num a) 
       => Ray a   -- ^ The ray to follow
       -> a       -- ^ The amount to follow the ray with
       -> Point a -- ^ The resulting point
follow (Ray o d) t = o <+^ t*^d
{-# INLINE follow #-}
