{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Lightning.Light
    ( LightSource (..)
    , Light (..)
    , PointLight (..)
    , LongRangePointLight (..)
    ) where

import RayTracer.Random
import RayTracer.Geometry
import RayTracer.Lightning.Spectrum


class (Shape a, Spectrum s) => LightSource a s where
    -- | Get the radiance caused by a light source at a specific point.
    getRadiance :: a            -- ^ The light source.
                -> Point Double -- ^ The point to get the radiance at.
                -> s            -- ^ The radiance at the point.
    
    getSample :: (MonadRandom m) => Int -> a -> Point Double -> m [(Point Double, s)]

data Light s = forall a. (LightSource a s, Show a) => Light a

instance Show (Light s) where
    show (Light l) = show l
instance Shape (Light s) where
    intersect ray (Light l) = intersect ray l
instance (Spectrum s) => LightSource (Light s) s where
    getRadiance (Light l) = getRadiance l
    getSample n (Light l) = getSample n l


data PointLight s
    -- | A point light with specified position and radiance.
    --   The intensity diminishes with 1/(squared distance).
    = PointLight 
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)

instance Shape (PointLight s) where
    intersect _ _ = Nothing
instance (Spectrum s) => LightSource (PointLight s) s where
    getRadiance (PointLight center spectrum) other = spectrum ^/ (normSqr $ other <-> center)
    getSample _ light@(PointLight center _) point = return [(center, getRadiance light point)]


data LongRangePointLight s
    -- | A point light with specified position and radiance that has a longer range than a regular point light.
    --   The intensity diminishes with 1/distance.
    = LongRangePointLight
        (Point Double) -- ^ The position of the light.
        s              -- ^ The radiance of the light at distance 1.
    deriving (Show)

instance Shape (LongRangePointLight s) where
    intersect _ _ = Nothing
instance (Spectrum s) => LightSource (LongRangePointLight s) s where
    getRadiance (LongRangePointLight center spectrum) other = spectrum ^/ (norm $ other <-> center)
    getSample _ light@(LongRangePointLight center _) point = return [(center, getRadiance light point)]
