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
import qualified Data.HashMap.Strict as H

-- | A class representing a camera that can generate a grid of rays.
class Camera a where
    -- | Trace all rays from a camera through a world and return the resulting image.
    rayTrace :: (RayTracer r s, Spectrum s)
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
        u :: Vector Double,
        v :: Vector Double,
        w :: Vector Double,
        width :: Double,
        height :: Double,
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
generateRays :: (MonadRandom m) => PerspectiveCamera -> m (Array D Ix2 (m [Ray Double]))
generateRays (PerspectiveCamera xRes yRes invXRes invYRes orig u v w width height strategy) = case strategy of
    RegularGrid n -> return $ makeArrayR D Par (Sz (yRes :. xRes)) generateCell
        where
            generateCell (r :. c) = return $ map (uncurry generateRay) points
                where
                    cornerX = (fromIntegral c) - (fromIntegral xRes)/2
                    cornerY = (fromIntegral (yRes - r)) - (fromIntegral yRes)/2
                    offset = 1/(2*(fromIntegral n))
                    points = [((cornerX + offset + ip)*invXRes, (cornerY - offset - jp)*invYRes) | i <- [0..(n-1)], j <- [0..(n-1)], let ip = (fromIntegral i) / (fromIntegral n), let jp = (fromIntegral j) / (fromIntegral n)]
    Stratified n -> return $ makeArrayR D Par (Sz (yRes :. xRes)) generateCell
        where
            generateCell (r :. c) = fmap (fmap (uncurry generateRay)) $ replicateM n $ do
                x <- getRandomR (cornerX, cornerX+1)
                y <- getRandomR (cornerY-1, cornerY)
                return (x*invXRes, y*invYRes)
                where
                    cornerX = (fromIntegral c) - (fromIntegral xRes)/2
                    cornerY = (fromIntegral (yRes - r)) - (fromIntegral yRes)/2
    Random n -> do
        indexMap <- fmap (H.fromListWith (+)) $ replicateM (xRes * yRes * n) $ do
            r <- getRandomR (0, yRes-1)
            c <- getRandomR (0, xRes-1)
            return ((r, c), 1)
        return $ makeArrayR D Par (Sz (yRes :. xRes)) (\(r :. c) -> generateCell ((r, c), H.lookupDefault 0 (r, c) indexMap))
        where
            generateCell ((r, c), count) = fmap (fmap (uncurry generateRay)) $ replicateM count $ do
                x <- getRandomR (cornerX, cornerX+1)
                y <- getRandomR (cornerY-1, cornerY)
                return (x*invXRes, y*invYRes)
                where
                    cornerX = (fromIntegral c) - (fromIntegral xRes)/2
                    cornerY = (fromIntegral (yRes - r)) - (fromIntegral yRes)/2
    where
        generateRay x y = createRay orig direction
            where
                uCoo = width * x
                vCoo = height * y
                direction = uCoo*^u ^+^ vCoo*^v ^-^ w


instance Camera PerspectiveCamera where
    rayTrace camera tracer world = do
        rays <- generateRays camera
        sequenceRand Par $ (`A.map` rays) $ \getRayCell -> do
            rayCell <- getRayCell
            cell <- mapM (traceRay tracer world) rayCell
            return $ averageV cell
