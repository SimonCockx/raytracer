module RayTracer.Core.Camera
    ( Camera (..)
    , AliasingStrategy (..)
    , PerspectiveCamera
    , createPerspectiveCamera
    ) where

import RayTracer.Random
import Data.Massiv.Array as A
import RayTracer.Geometry
import RayTracer.Core.RayTracer
import RayTracer.Core.World
import RayTracer.Lightning

-- | A class representing a camera that can generate a grid of rays.
class Camera a where
    -- | Trace all rays from a camera through a world and return the resulting image.
    rayTrace :: (MonadRandom m, MonadSplit g m, MonadRunner g m, RandomGen g, RayTracer r s, Spectrum s)
             => a                     -- ^ The camera to generate rays from
             -> r                     -- ^ The ray tracer to use
             -> World s               -- ^ The world to trace
             -> m (SpectralImage RGB) -- ^ The resulting image

data AliasingStrategy 
    = RegularGrid Int
    | Random Int
    | Jittered Int
    deriving (Show)

-- | A type representing a camera with perspective.
data PerspectiveCamera = 
    PerspectiveCamera {
        xResolution :: Int,
        yResolution :: Int,
        invXResolution :: Double,
        invYResolution :: Double,
        origin :: Point Double,
        u :: Vector Double,
        v :: Vector Double,
        w :: Vector Double,
        width :: Double,
        height :: Double,
        aliasingStrategy :: AliasingStrategy }
    deriving (Show)


-- | Create a perspective camera with given resolution, origin, lookat vector, up vector and field of view.
createPerspectiveCamera :: Int           -- ^ The x resolution
                        -> Int           -- ^ The y resolution
                        -> Point Double  -- ^ The origin of the camera
                        -> Vector Double -- ^ The lookat vector
                        -> Vector Double -- ^ The up vector
                        -> Double        -- ^ The field of view in radians
                        -> AliasingStrategy
                        -> PerspectiveCamera
createPerspectiveCamera xRes yRes origin lookAt up fov strategy
    | xRes < 1 || yRes < 1  = error "The resolution of a camera cannot be less than 1"
    | fov <= 0 || fov >= pi = error "The field of view must lie between 0 and pi"
    | l == 0                = error "The lookat vector and up vector are colinear"
    | otherwise = PerspectiveCamera xRes yRes invX invY origin u v w width height strategy
    where
        invX = (1.0 / fromIntegral xRes)
        invY = (1.0 / fromIntegral yRes)
        cr = lookAt `cross` up
        l = norm cr
        w = normalize $ negateV lookAt
        u = (1/l) *^ cr
        v = w `cross` u
        width = 2 * tan (fov/2)
        height = (fromIntegral yRes * width) * invX


-- | Generate the rays through this camera.
generateRays :: (MonadRandom m) => PerspectiveCamera -> Array D Ix2 (m (Ray Double))
generateRays (PerspectiveCamera xRes yRes invXRes invYRes orig u v w width height strategy) = case strategy of
    RegularGrid n -> makeArrayR D Par (Sz (n*yRes :. n*xRes)) generateRay
        where
        generateRay (r :. c) = return $ createRay orig direction
            where
                x = fromIntegral c / fromIntegral n
                y = fromIntegral (yRes - r) / fromIntegral n
                uCoo = width * (x * invXRes - 0.5) -- TODO: moet dit ni + 0.5 zijn???
                vCoo = height * (y * invYRes - 0.5)
                direction = uCoo*^u ^+^ vCoo*^v ^-^ w
    _ -> error "Not implemented aliasing strategy"


instance Camera PerspectiveCamera where
    rayTrace camera tracer world = do
        let rays = generateRays camera
        sequenceRand Par $ A.map (traceRay tracer world) rays
