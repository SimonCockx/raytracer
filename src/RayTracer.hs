module RayTracer
    ( RayTracer (..)
    , rayTrace
    , HitRayTracer (..)
    , LinearDepthRayTracer (..)
    , ExponentialDepthRayTracer (..)
    ) where

import Scene
import Camera
import Data.Massiv.Array.IO
import Data.Massiv.Array as A
import Graphics.ColorSpace
import Data.Maybe
import Shape
import Ray

-- | A class representing a ray tracer which can trace a ray through a scene and return the resulting color.
class RayTracer a where
    -- | Trace a ray through a given scene and return the resulting color.
    traceRay :: a -> Scene -> Ray -> Pixel RGB Word8

-- | Trace all rays from a camera through a scene and return the resulting image.
rayTrace :: (RayTracer tracer, Camera cam) 
         => tracer            -- ^ The ray tracer to use
         -> Scene             -- ^ The scene to trace
         -> cam               -- ^ The camera to generate rays from
         -> Image S RGB Word8 -- ^ The resulting image
rayTrace tracer scene camera = computeAs S $ A.map (traceRay tracer scene) $ generateRays camera


-- | A type representing a ray tracer that can detect hits.
data HitRayTracer = HitRayTracer

instance RayTracer HitRayTracer where
    traceRay _ scene ray
        | hit == Nothing = PixelRGB 0 0 0
        | otherwise = PixelRGB 255 0 0
        where
            hit = intersect ray scene


-- | A type representing a ray tracer that shows the depth of a scene linearly.
data LinearDepthRayTracer =
    -- | Construct a linear depth ray tracer with given minimum depth and maximum depth.
    --   The minimum depth will be mapped to white and the maximum depth will be mapped to black.
    --   All depths inbetween are interpolated linearly to grayscales. 
    LinearDepthRayTracer 
        Double -- ^ The minimum depth that will be mapped to white
        Double -- ^ The maximum depth that will be mapped to black

instance RayTracer LinearDepthRayTracer where
    traceRay (LinearDepthRayTracer min max) scene ray
        | hit == Nothing = PixelRGB 0 0 0
        | otherwise = let Just t = hit in toGray $ 1 - (t - min)/(max - min)
        where
            hit = intersect ray scene
            gray c = PixelRGB c c c


-- | A type representing a ray tracer that shows the depth of a scene exponentially.
data ExponentialDepthRayTracer = 
    -- | Construct an exponential depth ray tracer with given average depth.
    --   The average depth will be mapped to (0.5 * white).
    ExponentialDepthRayTracer
        Double -- ^ The average depth that will be mapped to (0.5 * white).

instance RayTracer ExponentialDepthRayTracer where
    traceRay (ExponentialDepthRayTracer average) scene ray
        | hit == Nothing = PixelRGB 0 0 0
        | otherwise = let Just t = hit in toGray $ 0.5**(t/average)
        where
            hit = intersect ray scene
            gray c = PixelRGB c c c

toWord :: Double -> Word8
toWord = fromIntegral . round . (255*) . (min 1) . (max 0)

toPixel :: Double -> Double -> Double -> Pixel RGB Word8
toPixel r g b = PixelRGB (toWord r) (toWord g) (toWord b)

toGray :: Double -> Pixel RGB Word8
toGray g = toPixel g g g
