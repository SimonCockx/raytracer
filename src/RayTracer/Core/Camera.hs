module RayTracer.Core.Camera
    ( Camera (..)
    , PerspectiveCamera
    , createPerspectiveCamera
    , generateRays
    ) where

import RayTracer.Random
import Data.Massiv.Array hiding (map, mapM)
import qualified Data.Massiv.Array as A
import RayTracer.Geometry
import RayTracer.Core.RayTracer
import RayTracer.Core.World
import RayTracer.Core.Sampling
import RayTracer.Lightning

-- | A class representing a camera that can generate a grid of rays.
class Camera a where
    -- | Trace all rays from a camera through a world and return the resulting image.
    rayTrace :: (RayTracer r s)
             => a                     -- ^ The camera to generate rays from
             -> r                     -- ^ The ray tracer to use
             -> World s               -- ^ The world to trace
             -> RandM (SpectralImage RGB) -- ^ The resulting image

-- | A type representing a camera with perspective.
data PerspectiveCamera = 
    PerspectiveCamera {
        xResolution :: Int,
        yResolution :: Int,
        invXResolution :: Double,
        invYResolution :: Double,
        origin :: Point Double,
        uVec :: Vector Double,
        vVec :: Vector Double,
        wVec :: Vector Double,
        getWidth :: Double,
        getHeight :: Double,
        samplingStrategy :: SamplingStrategy }
    deriving (Show)


-- | Create a perspective camera with given resolution, origin, lookat vector, up vector and field of view.
createPerspectiveCamera :: Int           -- ^ The x resolution
                        -> Int           -- ^ The y resolution
                        -> Point Double  -- ^ The origin of the camera
                        -> Vector Double -- ^ The lookat vector
                        -> Vector Double -- ^ The up vector
                        -> Double        -- ^ The field of view in radians
                        -> SamplingStrategy
                        -> PerspectiveCamera
createPerspectiveCamera xRes yRes orig lookAt up fov strategy
    | xRes < 1 || yRes < 1  = error "The resolution of a camera cannot be less than 1"
    | fov <= 0 || fov >= pi = error "The field of view must lie between 0 and pi"
    | l == 0                = error "The lookat vector and up vector are colinear"
    | otherwise = PerspectiveCamera xRes yRes invX invY orig u v w width height strategy
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
generateRays :: (MonadRandom m) => PerspectiveCamera -> Array D Ix2 (m [Ray Double])
generateRays (PerspectiveCamera xRes yRes invXRes invYRes orig u v w width height strategy) =
    makeArrayR D Par (Sz (yRes :. xRes)) generateCell
        where
            pixelWidth = width*invXRes
            pixelHeight = height*invYRes
            generateCell = (\(x, y) -> map (uncurry generateRay) <$> getSample strategy x y pixelWidth pixelHeight) . pixelCenter
            pixelCenter (r :. c) = (centerX, centerY)
                where
                    centerX = pixelWidth*((fromIntegral c) - (fromIntegral (xRes - 1))/2)
                    centerY = pixelHeight*((fromIntegral (yRes - 1 - r)) - (fromIntegral (yRes - 1))/2)
            generateRay uCoo vCoo = createRay orig direc
                where
                    direc = uCoo*^u ^+^ vCoo*^v ^-^ w


instance Camera PerspectiveCamera where
    rayTrace camera tracer world = do
        let rays = generateRays camera
        sequenceRand Par $ (`A.map` rays) $ \getRayCell -> do
            rayCell <- getRayCell
            cell <- mapM (traceRay tracer world) rayCell
            return $ averageV cell
