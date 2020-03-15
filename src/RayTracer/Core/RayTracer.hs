module RayTracer.Core.RayTracer
    ( RayTracer (..)
    , HitRayTracer (..)
    , LinearDepthRayTracer (..)
    , ExponentialDepthRayTracer (..)
    , NormalRayTracer (..)
    , IntersectionTestsTracer (..)
    , BVHLayerTracer (..)
    , SpectrumIndependentRayTracer (..)
    ) where

import RayTracer.Random

import RayTracer.Geometry
import RayTracer.Lightning
import RayTracer.Core.World
import RayTracer.Core.SceneObject
import RayTracer.Core.Sampling

-- | A class representing a ray tracer which can trace a ray through a world and return the resulting color.

class (Spectrum s) => RayTracer a s where
    -- | Trace a ray through a given world and return the resulting color.

    traceRay :: (MonadRandom m) => a -> World s -> Ray Double -> m RGB


traceHittingRay :: (Spectrum out, MonadRandom m, Shape a) => (Double -> Vector Double -> out) -> a -> Ray Double -> m RGB
traceHittingRay onHit shape ray =
    case intersect ray shape of
        Nothing -> return black
        Just (t, n) -> return $ toRGB $ onHit t n

-- | A type representing a ray tracer that can detect hits.

data HitRayTracer = HitRayTracer

instance (Spectrum s) => RayTracer HitRayTracer s where
    traceRay HitRayTracer = traceHittingRay $ \_ _ -> RGB 1 0 0


-- | A type representing a ray tracer that shows the depth of a world linearly.

data LinearDepthRayTracer =
    -- | Construct a linear depth ray tracer with given minimum depth and maximum depth.

    --   The minimum depth will be mapped to white and the maximum depth will be mapped to black.

    --   All depths inbetween are interpolated linearly to grayscales. 

    LinearDepthRayTracer
        Double -- ^ The minimum depth that will be mapped to white

        Double -- ^ The maximum depth that will be mapped to black


instance (Spectrum s) => RayTracer LinearDepthRayTracer s where
    traceRay (LinearDepthRayTracer minD maxD) = traceHittingRay $ \t _ -> Gray $ 1 - (t - minD)/(maxD - minD)


-- | A type representing a ray tracer that shows the depth of a world exponentially.

newtype ExponentialDepthRayTracer =
    -- | Construct an exponential depth ray tracer with given average depth.

    --   The average depth will be mapped to (0.5 * white).

    ExponentialDepthRayTracer
        Double -- ^ The average depth that will be mapped to (0.5 * white).


instance (Spectrum s) => RayTracer ExponentialDepthRayTracer s where
    traceRay (ExponentialDepthRayTracer average) = traceHittingRay $ \t _ -> Gray $ 0.5**(t/average)


-- | A type representing a ray tracer that shows the shading normals of a world relative to the ray by color.

data NormalRayTracer = NormalRayTracer

instance (Spectrum s) => RayTracer NormalRayTracer s where
    traceRay NormalRayTracer = traceHittingRay $ \_ (Vector x y z) -> (RGB x y z ^+^ white) ^/ 2


newtype IntersectionTestsTracer = IntersectionTestsTracer Int

instance (Spectrum s) => RayTracer IntersectionTestsTracer s where
    traceRay (IntersectionTestsTracer cap) world ray
        | p < 1     = return $ RGB 0 p 1
        | p < 2     = return $ RGB 0 1 (2-p)
        | p < 3     = return $ RGB (p-2) 1 0
        | p < 4     = return $ RGB 1 (4-p) 0
        | otherwise = return $ RGB 1 0 0
        where
            n = numberOfIntersectionTests ray world
            p = 4*fromIntegral n / fromIntegral cap


newtype BVHLayerTracer = BVHLayerTracer Int

instance (Spectrum s) => RayTracer BVHLayerTracer s where
    traceRay (BVHLayerTracer depth) world ray = traceHittingRay (\_ n -> (n <.> negateV (direction ray)) *^ RGB 0 0.5 1) boxes ray
        where
            bvhs = map boundingVolume $ objects world
            extractBoxes 0 bvh = [simpleObject $ boundingBox bvh :: SceneObject Gray]
            extractBoxes d bvh = case bvh of
                BoundingVolume _ innerVolumes -> concatMap (extractBoxes $ d-1) innerVolumes
                Bounded _ _ -> []
                TransformedBoundingVolume _ t innerVolume -> [simpleObject $ Transformed t $ extractBoxes (d-1) innerVolume]
            boxes = concatMap (extractBoxes depth) bvhs


-- | The offset for a shadow ray to remove self-shadow artefacts.

selfShadowFactor :: Double
selfShadowFactor = 1e-10

shadowRay :: Point Double -> Point Double -> Ray Double
shadowRay p1 p2 = let r = createRay p1 (p2 <-> p1) in Ray ((origin r) <+^ selfShadowFactor*^(direction r)) (direction r)

isVisible :: Point Double -> Point Double -> World s -> Bool
isVisible p1 p2 world = case intersect (shadowRay p1 p2) world of
    Nothing     -> True
    Just (t, _) -> (t + selfShadowFactor)*(t + selfShadowFactor) >= normSqr (p2 <-> p1)

-- | A type representing a ray tracer that traces direct lightning for any spectrum.

newtype SpectrumIndependentRayTracer = SpectrumIndependentRayTracer SamplingStrategy

filterRadianceSample :: (Spectrum s) => [(Point Double, s)] -> World s -> Ray Double -> SceneObject s -> Point Double -> Vector Double -> s
filterRadianceSample sample world ray obj point normal = sumV radiances
    where
        l_out = direction ray
        visibleSample = filter (\(p, _) -> isVisible point p world) sample
        radiances = map (\(p, s) -> let l_in = normalize $ p <-> point in
                                    max 0 (l_in <.> normal) *^ brdf obj point l_in l_out ^*^ s) visibleSample


instance (Spectrum s) => RayTracer SpectrumIndependentRayTracer s where
    traceRay (SpectrumIndependentRayTracer strat) world ray = do
        pixel <- case findHit ray $ objects world of
                 Nothing -> return black
                 Just (obj, (t, n)) -> do
                     let p  = follow ray t
                         le = getRadiance obj p (origin ray)
                     samples <- mapM (\light -> generateSample strat light p) $ lights world
                     return $ foldr ((^+^) . (\sample -> filterRadianceSample sample world ray obj p n)) le samples
        return $ toRGB $ gammaCorrect pixel

