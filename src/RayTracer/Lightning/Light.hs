module RayTracer.Lightning.Light
    ( LightSource (..)
    , Light (..)
    , AmbientLight (..)
    , PointLight (..)
    , LongRangePointLight (..)
    , AreaLight (..)
    ) where

import RayTracer.Random
import RayTracer.Geometry
import RayTracer.Core.Sampling
import RayTracer.Lightning.Spectrum


class (Shape a, Spectrum s) => LightSource a s where
    -- | Get the radiance caused by a light source at a specific point.
    getRadiance :: a            -- ^ The light source.
                -> Point Double -- ^ A point on the light source.
                -> Point Double -- ^ The point to get the radiance at.
                -> s            -- ^ The radiance at the point.
    
    generateSample :: (MonadRandom m) => SamplingStrategy -> a -> Point Double -> m [(Point Double, s)]

data Light s = forall a. (LightSource a s, Boundable a, Shape ~ BoundedContent a, Show a) => Light a

instance Show (Light s) where
    show (Light l) = show l
-- instance Boundable (Light s) where
--     type BoundedContent (Light s) = Shape
--     boundingBox (Light l) = boundingBox l
--     boundingVolume (Light l) = boundingVolume l
instance Shape (Light s) where
    intersect ray (Light l) = intersect ray l
instance (Spectrum s, Spectrum a, s ~ a) => LightSource (Light s) a where
    getRadiance (Light l) = getRadiance l
    generateSample strat (Light l) = generateSample strat l


instance (LightSource l s) => LightSource (Transformed l) s where
    getRadiance (Transformed t light) pl pt = getRadiance light (t `inverseTransform` pl) (t `inverseTransform` pt)
    generateSample strat (Transformed t light) p = fmap (map (\(point, spec) -> (t `transform` point, spec))) sample
        where
            sample = generateSample strat light (t `inverseTransform` p)


data AmbientLight s = AmbientLight s
    deriving (Show)
instance (Show s) => Boundable (AmbientLight s) where
    type BoundedContent (AmbientLight s) = Shape
    boundingBox _ = createAABB (pure 1) (pure (-1))
instance (Show s) => Shape (AmbientLight s) where
    intersect _ _ = Nothing
instance (Spectrum s1, Show s1, s1 ~ s2) => LightSource (AmbientLight s1) s2 where
    getRadiance (AmbientLight spectrum) _ _ = spectrum
    generateSample _ (AmbientLight spectrum) point = return [(point, spectrum)]


data PointLight s
    -- | A point light with specified position and radiance.
    --   The intensity diminishes with 1/(squared distance).
    = PointLight 
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)

instance (Show s) => Boundable (PointLight s) where
    type BoundedContent (PointLight s) = Shape
    boundingBox _ = createAABB (pure 1) (pure (-1))
instance (Show s) => Shape (PointLight s) where
    intersect _ _ = Nothing
instance (Show s1, Spectrum s1, s1 ~ s2) => LightSource (PointLight s1) s2 where
    getRadiance (PointLight center spectrum) _ other = spectrum ^/ (normSqr $ other <-> center)
    generateSample _ light@(PointLight center _) point = return [(center, getRadiance light center point)]


data LongRangePointLight s
    -- | A point light with specified position and radiance that has a longer range than a regular point light.
    --   The intensity diminishes with 1/distance.
    = LongRangePointLight
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)

instance (Show s) => Boundable (LongRangePointLight s) where
    type BoundedContent (LongRangePointLight s) = Shape
    boundingBox _ = createAABB (pure 1) (pure (-1))
instance (Show s) => Shape (LongRangePointLight s) where
    intersect _ _ = Nothing
instance (Show s1, Spectrum s1, s1 ~ s2) => LightSource (LongRangePointLight s1) s2 where
    getRadiance (LongRangePointLight center spectrum) _ other = spectrum ^/ (norm $ other <-> center)
    generateSample _ light@(LongRangePointLight center _) point = return [(center, getRadiance light center point)]


data AreaLight s = AreaLight Double Double s
    deriving (Show)

instance (Show s) => Boundable (AreaLight s) where
    type BoundedContent (AreaLight s) = Shape
    boundingBox (AreaLight width height _) = createAABB (Point (-width/2) 0 (-height/2)) (Point (width/2) 0 (height/2))
instance (Show s) => Shape (AreaLight s) where
    intersect ray (AreaLight width height _)
        | -width/2 <= x && x <= width/2 && -height/2 <= z && z <= height/2 = Just (t, normal, Vector (x/width) (z/height) 0)
        | otherwise = Nothing
        where
            t = -yo/yd
            normal = Vector 0 (negate $ signum yd) 0
            Point x _ z = follow ray t
            Point _ yo _ = origin ray
            Vector _ yd _ = direction ray

getRadianceDividedByDistSqr :: (Spectrum s) => AreaLight s -> Point Double -> Point Double -> s
getRadianceDividedByDistSqr (AreaLight width height spec) pl pt = spec ^* (width * height * (abs y) / distSqr)
        where
            dp = pt <-> pl
            distSqr = normSqr dp
            d = dp ^/ (sqrt distSqr)
            Vector _ y _ = d
instance (Show s1, Spectrum s1, s1 ~ s2) => LightSource (AreaLight s1) s2 where
    getRadiance (AreaLight width height spec) pl pt = spec ^* (width * height * (abs y))
        where
            dp = pt <-> pl
            distSqr = normSqr dp
            d = dp ^/ (sqrt distSqr)
            Vector _ y _ = d
    generateSample strat light@(AreaLight width height _) point = do
        rOffsets <- getSample strat
        let points = map (\(rx, rz) -> Point (rx*width) 0 (rz*height)) rOffsets
            count = fromIntegral $ length points
        return $ zip points $ map (\pl -> getRadianceDividedByDistSqr light pl point ^/ count) points 
