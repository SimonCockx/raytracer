module RayTracer.Core.RayTracer
    ( RayTracer (..)
    , HitRayTracer (..)
    , LinearDepthRayTracer (..)
    , ExponentialDepthRayTracer (..)
    , NormalRayTracer (..)
    , SpectrumIndependentRayTracer (..)
    ) where

import RayTracer.Random
import Control.Monad.State

import RayTracer.Geometry
import RayTracer.Lightning
import RayTracer.Core.World
import RayTracer.Core.SceneObject
import qualified Data.Massiv.Array as A
import Data.Massiv.Array hiding (map, mapM)
import Data.Maybe

-- | A class representing a ray tracer which can trace a ray through a world and return the resulting color.
class (Spectrum s) => RayTracer a s where
    -- | Trace a ray through a given world and return the resulting color.
    traceRay :: (MonadRandom m) => a -> World s -> m (Ray Double) -> m RGB


traceHittingRay :: (Show s, Spectrum s, Spectrum out, MonadRandom m) => (Double -> Vector Double -> out) -> World s -> m (Ray Double) -> m RGB
traceHittingRay onHit world getRay = do
    ray <- getRay
    case intersect ray world of
        Nothing -> return black
        Just (t, n) -> return $ toRGB $ onHit t n

-- | A type representing a ray tracer that can detect hits.
data HitRayTracer = HitRayTracer

instance (Show s, Spectrum s) => RayTracer HitRayTracer s where
    traceRay HitRayTracer = traceHittingRay $ \_ _ -> RGB 1 0 0


-- | A type representing a ray tracer that shows the depth of a world linearly.
data LinearDepthRayTracer =
    -- | Construct a linear depth ray tracer with given minimum depth and maximum depth.
    --   The minimum depth will be mapped to white and the maximum depth will be mapped to black.
    --   All depths inbetween are interpolated linearly to grayscales. 
    LinearDepthRayTracer 
        Double -- ^ The minimum depth that will be mapped to white
        Double -- ^ The maximum depth that will be mapped to black

instance (Show s, Spectrum s) => RayTracer LinearDepthRayTracer s where
    traceRay (LinearDepthRayTracer min max) = traceHittingRay $ \t _ -> Gray $ 1 - (t - min)/(max - min)


-- | A type representing a ray tracer that shows the depth of a world exponentially.
newtype ExponentialDepthRayTracer = 
    -- | Construct an exponential depth ray tracer with given average depth.
    --   The average depth will be mapped to (0.5 * white).
    ExponentialDepthRayTracer
        Double -- ^ The average depth that will be mapped to (0.5 * white).

instance (Show s, Spectrum s) => RayTracer ExponentialDepthRayTracer s where
    traceRay (ExponentialDepthRayTracer average) = traceHittingRay $ \t _ -> Gray $ 0.5**(t/average)


-- | A type representing a ray tracer that shows the shading normals of a world relative to the ray by color.
data NormalRayTracer = NormalRayTracer

instance (Show s, Spectrum s) => RayTracer NormalRayTracer s where
    traceRay NormalRayTracer = traceHittingRay $ \_ (Vector x y z) -> (RGB x y z ^+^ white) ^/ 2

-- | The offset for a shadow ray to remove self-shadow artefacts.
selfShadowFactor :: Double
selfShadowFactor = 1e-10

shadowRay p1 p2 = let r = createRay p1 (p2 <-> p1) in Ray ((origin r) <+^ selfShadowFactor*^(direction r)) (direction r)
isVisible p1 p2 world = case intersect (shadowRay p1 p2) world of
    Nothing     -> True
    Just (t, _) -> t*t >= normSqr (p2 <-> p1)

-- | A type representing a ray tracer that traces direct lightning for any spectrum.
data SpectrumIndependentRayTracer = SpectrumIndependentRayTracer

filterRadianceSample :: (Spectrum s, Show s) => [(Point Double, s)] -> World s -> Ray Double -> SceneObject s -> Point Double -> Vector Double -> s
filterRadianceSample sample world ray obj point normal = sumV radiances
    where
        l_out = direction ray
        visibleSample = filter (\(p, _) -> isVisible point p world) sample
        radiances = map (\(p, s) -> let l_in = normalize $ p <-> point in
                                    max 0 (l_in <.> normal) *^ brdf obj point l_in l_out ^*^ s) visibleSample


instance (Spectrum s, Show s) => RayTracer SpectrumIndependentRayTracer s where
    traceRay SpectrumIndependentRayTracer world getRay = do
        ray <- getRay
        pixel <- case findHit ray $ objects world of
                 Nothing -> return black
                 Just (obj, (t, n)) -> do
                     let p = follow ray t
                     samples <- mapM (\light -> getSample 1 light p) $ lights world
                     return $ sumV $ map (\sample -> filterRadianceSample sample world ray obj p n) samples
        return $ toRGB $ gammaCorrect pixel
