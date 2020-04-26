module RayTracer.Lightning.Light
    ( LightSample
    , LightSource (..)
    , Light (..)
    , AmbientLight (..)
    , PointLight (..)
    , LongRangePointLight (..)
    , createAreaLight
    ) where

import RayTracer.Random
import RayTracer.Geometry
import RayTracer.Core.Sampling
import RayTracer.Lightning.Spectrum

type LightSample s = (Point Double, s)

class (Shape a) => LightSource a s where
    -- | Get the radiance caused by a light source at a specific point.
    getRadiance :: a            -- ^ The light source.
                -> Point Double -- ^ A point on the light source.
                -> Point Double -- ^ The point to get the radiance at.
                -> s            -- ^ The radiance at the point.
    
    generateSample :: (MonadRandom m) => SamplingStrategy -> a -> Point Double -> m [LightSample s]

data Light s = forall a. (LightSource a s, Show a) => Light a

instance Show (Light s) where
    show (Light l) = show l
instance Shape (Light s) where
    intersect ray (Light shape) = intersect ray shape
    numberOfIntersectionTests ray (Light shape) = numberOfIntersectionTests ray shape
    boundingBox (Light shape) = boundingBox shape
    boundedNode (Light shape) = boundedNode shape
instance (s1 ~ s2) => LightSource (Light s1) s2 where
    getRadiance (Light l) = getRadiance l
    generateSample strat (Light l) = generateSample strat l


instance (LightSource l s) => LightSource (Transformed l) s where
    getRadiance (Transformed t light) pl pt = getRadiance light (t `inverseTransform` pl) (t `inverseTransform` pt)
    generateSample strat (Transformed t light) p = fmap (map (\(point, spec) -> (t `transform` point, spec))) sample
        where
            sample = generateSample strat light (t `inverseTransform` p)


data AmbientLight s = AmbientLight s
    deriving (Show)
instance (Show s) => Shape (AmbientLight s) where
    intersect _ _ = Nothing
    numberOfIntersectionTests _ _ = 0
    boundingBox _ = createAABB (Point 1 1 1) (pure (-1))
    boundedNode = UnboundedNode
instance (s1 ~ s2, Show s1) => LightSource (AmbientLight s1) s2 where
    getRadiance (AmbientLight spectrum) _ _ = spectrum
    generateSample _ (AmbientLight spectrum) point = return [(point, spectrum)]


data PointLight s
    -- | A point light with specified position and radiance.
    --   The intensity diminishes with 1/(squared distance).
    = PointLight 
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)
instance (Show s) => Shape (PointLight s) where
    intersect _ _ = Nothing
    numberOfIntersectionTests _ _ = 0
    boundingBox _ = createAABB (Point 1 1 1) (pure (-1))
    boundedNode = UnboundedNode
instance (Spectrum s1, s1 ~ s2, Show s1) => LightSource (PointLight s1) s2 where
    getRadiance (PointLight center spectrum) _ other = spectrum ^/ (normSqr $ other <-> center)
    generateSample _ light@(PointLight center _) point = return [(center, getRadiance light center point)]


data LongRangePointLight s
    -- | A point light with specified position and radiance that has a longer range than a regular point light.
    --   The intensity diminishes with 1/distance.
    = LongRangePointLight
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)
instance (Show s) => Shape (LongRangePointLight s) where
    intersect _ _ = Nothing
    numberOfIntersectionTests _ _ = 0
    boundingBox _ = createAABB (Point 1 1 1) (pure (-1))
    boundedNode = UnboundedNode
instance (Spectrum s1, s1 ~ s2, Show s1) => LightSource (LongRangePointLight s1) s2 where
    getRadiance (LongRangePointLight center spectrum) _ other = spectrum ^/ (norm $ other <-> center)
    generateSample _ light@(LongRangePointLight center _) point = return [(center, getRadiance light center point)]


data AreaLight s = AreaLight Double Double (Transformed Rectangle) s
    deriving (Show)

createAreaLight :: Vector Double -> Double -> Double -> s -> Transformed (AreaLight s)
createAreaLight normal width height spec = 
    Transformed (inverse $ alignWithXAxis normal) (AreaLight width height (createRectangle (Vector 1 0 0) width height) spec)

instance (Show s) => Shape (AreaLight s) where
    intersect ray (AreaLight _ _ rect _) = intersect ray rect
    numberOfIntersectionTests ray (AreaLight _ _ rect _) = numberOfIntersectionTests ray rect
    boundingBox (AreaLight _ _ rect _) = boundingBox rect
    boundedNode (AreaLight _ _ rect _) = boundedNode rect

getRadianceDividedByDistSqr :: (Spectrum s) => AreaLight s -> Point Double -> Point Double -> s
getRadianceDividedByDistSqr (AreaLight width height _ spec) pl pt = spec ^* (width * height * abs xCos / distSqr)
        where
            dp = pt <-> pl
            distSqr = normSqr dp
            d = dp ^/ sqrt distSqr
            Vector xCos _ _ = d
instance (Spectrum s1, s1 ~ s2, Show s1) => LightSource (AreaLight s1) s2 where
    getRadiance (AreaLight width height _ spec) pl pt = spec ^* (width * height * abs xCos)
        where
            dp = pt <-> pl
            distSqr = normSqr dp
            d = dp ^/ sqrt distSqr
            Vector xCos _ _ = d
    generateSample strat light@(AreaLight width height _ _) point = do
        rOffsets <- getSample strat 0 0 width height
        let points = map (\(rz, ry) -> Point 0 ry rz) rOffsets
            count = fromIntegral $ length points
        return $ zip points $ map (\pl -> getRadianceDividedByDistSqr light pl point ^/ count) points 
