module RayTracer.Core.RayTracer
    ( RayTracer (..)
    , HitRayTracer (..)
    , LinearDepthRayTracer (..)
    , ExponentialDepthRayTracer (..)
    , NormalRayTracer (..)
    , IntersectionTestsTracer (..)
    , DiffuseRayTracer (..)
    , DirectLightningTracer (..)
    , SpecificDepthPathTracer (..)
    , MaxDepthPathTracer (..)
    , RussianRoulettePathTracer (..)
    ) where

import RayTracer.Random

import RayTracer.Geometry
import RayTracer.Lightning
import RayTracer.Core.World
import RayTracer.Core.SceneObject
import RayTracer.Core.Sampling


-- | A class representing a ray tracer which can trace a ray through a world and return the resulting color.
class RayTracer a s where
    -- | Trace a ray through a given world and return the resulting color.
    traceRay :: (MonadRandom m) => a -> World s -> Ray Double -> m RGB


traceHittingRay :: (Spectrum out, MonadRandom m, Shape a) => (Double -> Vector Double -> out) -> a -> Ray Double -> m RGB
traceHittingRay onHit shape ray =
    case intersect ray shape of
        Nothing -> return black
        Just (t, n, _) -> return $ toRGB $ onHit t n

-- | A type representing a ray tracer that can detect hits.
data HitRayTracer = HitRayTracer

instance RayTracer HitRayTracer s where
    traceRay HitRayTracer = traceHittingRay $ \_ _ -> RGB 1 0 0


-- | A type representing a ray tracer that shows the depth of a world linearly.
data LinearDepthRayTracer =
    -- | Construct a linear depth ray tracer with given minimum depth and maximum depth.
    --   The minimum depth will be mapped to white and the maximum depth will be mapped to black.
    --   All depths inbetween are interpolated linearly to grayscales. 
    LinearDepthRayTracer
        Double -- ^ The minimum depth that will be mapped to white
        Double -- ^ The maximum depth that will be mapped to black


instance RayTracer LinearDepthRayTracer s where
    traceRay (LinearDepthRayTracer minD maxD) = traceHittingRay $ \t _ -> Gray $ 1 - (t - minD)/(maxD - minD)


-- | A type representing a ray tracer that shows the depth of a world exponentially.
newtype ExponentialDepthRayTracer =
    -- | Construct an exponential depth ray tracer with given average depth.
    --   The average depth will be mapped to (0.5 * white).
    ExponentialDepthRayTracer
        Double -- ^ The average depth that will be mapped to (0.5 * white).


instance RayTracer ExponentialDepthRayTracer s where
    traceRay (ExponentialDepthRayTracer average) = traceHittingRay $ \t _ -> Gray $ 0.5**(t/average)


-- | A type representing a ray tracer that shows the shading normals of a world by color.
data NormalRayTracer = NormalRayTracer

instance RayTracer NormalRayTracer s where
    traceRay NormalRayTracer = traceHittingRay $ \_ (Vector x y z) -> (RGB x y z ^+^ white) ^/ 2


newtype IntersectionTestsTracer = IntersectionTestsTracer Int

instance RayTracer IntersectionTestsTracer s where
    traceRay (IntersectionTestsTracer cap) world ray
        | p < 1     = return $ RGB 0 p 1
        | p < 2     = return $ RGB 0 1 (2-p)
        | p < 3     = return $ RGB (p-2) 1 0
        | p < 4     = return $ RGB 1 (4-p) 0
        | otherwise = return $ RGB 1 0 0
        where
            n = numberOfIntersectionTests ray world
            p = 4*fromIntegral n / fromIntegral cap


data DiffuseRayTracer = DiffuseRayTracer

instance (Spectrum s) => RayTracer DiffuseRayTracer s where
    traceRay _ world ray = do
        pixel <- case findInspectingHit ray world of
                 Nothing -> return $ worldBackground world (direction ray)
                 Just (InspectingHit _ brdf (_, n, _)) -> do
                     let l_out = negateV $ direction ray
                     return $ max 0 (l_out <.> n) *^ brdf l_out l_out
        return $ gammaCorrect $ toRGB pixel



isVisible :: Point Double -> Point Double -> World s -> Bool
isVisible p1 p2 world
    | p1 == p2  = True
    | otherwise = case intersect (createRay p1 (p2 <-> p1)) world of
        Nothing     -> True
        Just (t, _, _) -> (t + selfShadowFactor)*(t + selfShadowFactor) >= normSqr (p2 <-> p1)


-- | A type representing a ray tracer that traces direct lightning for any spectrum.
newtype DirectLightningTracer = DirectLightningTracer SamplingStrategy

sampleLight :: (MonadRandom m, Spectrum s) => MaterialHit s -> Ray Double -> SamplingStrategy -> [Light s] -> World s -> m (s, s)
sampleLight matHit ray strat lights world = do
    ShadowHit emit reflSamples <- shadowReflectHit matHit strat lights
    let InspectingHit _ _ (t, normal, _) = inspectHit matHit
        point = shadowPoint (follow ray t) normal
        visibleSamples = filter (\(p, _) -> isVisible point p world) reflSamples
        radiances = map snd visibleSamples
    return (emit, sumV radiances)


instance (Spectrum s) => RayTracer DirectLightningTracer s where
    traceRay (DirectLightningTracer strat) world ray = do
        pixel <- case findHit ray world of
            Nothing -> return $ worldBackground world (direction ray)
            Just matHit -> uncurry (^+^) <$> sampleLight matHit ray strat (worldLights world) world
        return $ gammaCorrect $ toRGB pixel


-- A help functions for path tracers

sampleSingleLight :: (MonadRandom m, Spectrum s) => MaterialHit s -> Ray Double -> World s -> m (s, s)
sampleSingleLight matHit ray world = do
    light <- (ls !!) <$> getRandomR (0, cl - 1)
    (emit, spec) <- sampleLight matHit ray (Random 1) [light] world
    return (emit, fromIntegral cl *^ spec)
        where
            ls = worldLights world
            cl = length ls

traceShadowRay :: (MonadRandom m, Spectrum s) => Ray Double -> World s -> m (s, s)
traceShadowRay ray world = case findHit ray world of
    Nothing     -> return (worldBackground world (direction ray), black)
    Just matHit -> sampleSingleLight matHit ray world


-- | A type representing a path tracer that traces rays for a specific depth.
newtype SpecificDepthPathTracer = SpecificDepthPathTracer Int

traceDepthReflectanceRay :: (MonadRandom m, Spectrum s) => Int -> Ray Double -> World s -> m (s, s)
traceDepthReflectanceRay 0 ray world = traceShadowRay ray world
traceDepthReflectanceRay depth ray world = case findReflectingHit ray world of
    Nothing -> return (worldBackground world (direction ray), black)
    Just mHit -> do
        ReflectingHit _ refl newRay <- mHit
        (_, spec) <- traceDepthReflectanceRay (depth - 1) newRay world
        return (black, refl ^*^ spec)

instance (Spectrum s) => RayTracer SpecificDepthPathTracer s where
    traceRay (SpecificDepthPathTracer depth) world ray = do
        (emit, spec) <- traceDepthReflectanceRay depth ray world
        return $ gammaCorrect $ toRGB $ emit ^+^ spec


-- | A type representing a path tracer that traces rays for a max depth and samples light at each intersection.
newtype MaxDepthPathTracer = MaxDepthPathTracer Int

traceMaxDepthReflectanceRay :: (MonadRandom m, Spectrum s) => Int -> Ray Double -> World s -> m (s, s)
traceMaxDepthReflectanceRay 0 ray world = traceShadowRay ray world
traceMaxDepthReflectanceRay depth ray world = case findHit ray world of
    Nothing -> return (worldBackground world (direction ray), black)
    Just matHit -> do
        (_, lightSpec) <- sampleSingleLight matHit ray world
        ReflectingHit emit refl newRay <- reflectHit matHit
        (_, spec) <- traceMaxDepthReflectanceRay (depth - 1) newRay world
        return (emit, lightSpec ^+^ refl ^*^ spec)

instance (Spectrum s) => RayTracer MaxDepthPathTracer s where
    traceRay (MaxDepthPathTracer depth) world ray = do
        (emit, spec) <- traceMaxDepthReflectanceRay depth ray world
        return $ gammaCorrect $ toRGB $ emit ^+^ spec


-- | A type representing a path tracer with Russian Roulette.
newtype RussianRoulettePathTracer = RussianRoulettePathTracer Double

traceRouletteReflectanceRay :: (MonadRandom m, Spectrum s) => Double -> Ray Double -> World s -> m (s, s)
traceRouletteReflectanceRay alpha ray world = do
    x <- getRandom
    if x < alpha then do
        (emit, spec) <- traceShadowRay ray world
        return (emit, (1/alpha) *^ spec)
    else case findReflectingHit ray world of
        Nothing -> return (black, black)
        Just mHit -> do
            ReflectingHit emit refl newRay <- mHit
            (_, spec) <- traceRouletteReflectanceRay alpha newRay world
            return (emit, (1/(1 - alpha)) *^ (refl ^*^ spec))

instance (Spectrum s) => RayTracer RussianRoulettePathTracer s where
    traceRay (RussianRoulettePathTracer alpha) world ray = do
        (emit, spec) <- traceRouletteReflectanceRay alpha ray world
        return $ gammaCorrect $ toRGB $ (emit ^+^ spec)




