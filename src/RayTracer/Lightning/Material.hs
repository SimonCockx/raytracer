{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Lightning.Material
    ( Material (..)
    , WhiteMaterial (..)
    , BlackMaterial (..)
    , Diffuse (..)
    ) where

import RayTracer.Geometry
import RayTracer.Lightning.Spectrum

-- | A class representing a material concerning a bidirectional reflectance distribution function (brdf) of a specific spectrum.
class Material m s where
    -- | The brdf of this material.
    brdf :: m
         -> Point Double  -- ^ The point to get the brdf at
         -> Vector Double -- ^ The direction of the incomming radiance
         -> Vector Double -- ^ The reflected direction
         -> s             -- ^ The reflectance in the specified direction

-- | A type that represents a diffuse, white material.
data WhiteMaterial = WhiteMaterial
    deriving (Show)

instance (Spectrum s) => Material WhiteMaterial s where
    brdf WhiteMaterial _ _ _ = white

-- | A type that represents a diffuse, black material.
data BlackMaterial = BlackMaterial
    deriving (Show)

instance (Spectrum s) => Material BlackMaterial s where
    brdf BlackMaterial _ _ _ = black

-- | A type that represents a diffuse material of a specific spectrum.
data Diffuse s
    -- | A diffuse material with a specified reflectance.
    = Diffuse
        s -- ^ The reflectance of this material
          --   White represents 100% reflectance.
    deriving (Show)

instance (Spectrum s) => Material (Diffuse s) s where
    brdf (Diffuse reflectance) _ _ _ = reflectance
