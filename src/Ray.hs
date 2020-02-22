module Ray 
    ( Ray (origin, direction)
    , createRay
    ) where

import Vector

-- | A type representing a ray.
data Ray
    -- | Construct a ray with given origin and direction.
    = Ray {
        -- | Return the origin of the ray.
        origin :: Point Double,
        -- | Return the normalized direction of the ray.
        direction :: Vector Double}

-- | Creates a new ray with given origin and direction.
--   The direction is normalized in the process.
createRay :: Point Double  -- ^ The origin
          -> Vector Double -- ^ The direction
          -> Ray           -- ^ The resulting ray
createRay origin direction = Ray origin (normalize direction)
