module Camera
    ( Camera (..)
    , PerspectiveCamera
    , createPerspectiveCamera
    ) where

import Data.Massiv.Array as A
import Ray
import Vector

-- | A class representing a camera that can generate a grid of rays.
class Camera a where
    -- | Generate the rays through this camera.
    generateRays :: a -> Array D Ix2 Ray

-- | A type representing a camera with perspective.
data PerspectiveCamera = 
    PerspectiveCamera {
        xResolution :: Int,
        yResolution :: Int,
        invXResolution :: Double,
        invYResolution :: Double,
        origin :: Point,
        u :: Vector,
        v :: Vector,
        w :: Vector,
        width :: Double,
        height :: Double }
        deriving Show


-- | Create a perspective camera with given resolution, origin, lookat vector, up vector and field of view.
createPerspectiveCamera :: Int    -- ^ The x resolution
                        -> Int    -- ^ The y resolution
                        -> Point  -- ^ The origin of the camera
                        -> Vector -- ^ The lookat vector
                        -> Vector -- ^ The up vector
                        -> Double -- ^ The field of view in radians
                        -> PerspectiveCamera
createPerspectiveCamera xRes yRes origin lookAt up fov
    | xRes < 1 || yRes < 1  = error "The resolution of a camera cannot be less than 1"
    | fov <= 0 || fov >= pi = error "The field of view must lie between 0 and pi"
    | l == 0                = error "The lookat vector and up vector are colinear"
    | otherwise = PerspectiveCamera xRes yRes invX invY origin u v w width height
    where
        invX = (1.0 / fromIntegral xRes)
        invY = (1.0 / fromIntegral yRes)
        cr = invert (up `cross` lookAt)
        l = norm cr
        w = normalize $ invert lookAt
        u = (1/l) .*> cr
        v = w `cross` u
        width = 2 * tan (fov/2)
        height = (fromIntegral yRes * width) * invX


instance Camera PerspectiveCamera where
    generateRays (PerspectiveCamera xRes yRes invXRes invYRes orig u v w width height) =
        makeArrayR D Par (Sz (xRes :. yRes)) generateRay
        where
            generateRay (r :. c) = createRay orig direction
                where
                    x = c
                    y = yRes - r
                    uCoo = width * (fromIntegral x * invXRes - 0.5)
                    vCoo = height * (fromIntegral y * invYRes - 0.5)
                    direction = uCoo .*> u <+> vCoo .*> v <-> w
