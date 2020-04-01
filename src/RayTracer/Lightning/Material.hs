{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Lightning.Material
    ( BRDF
    , Material (..)
    , WhiteMaterial (..)
    , BlackMaterial (..)
    , Diffuse (..)
    , TextureMap
    , DiffuseTexture (..)
    , readTextureMap
    , ProceduralDiffuseTexture (..)
    ) where

import RayTracer.Geometry
import RayTracer.Lightning.Spectrum
import Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as MIO

type BRDF s = Vector Double -> Vector Double -> Vector Double -> s

-- | A class representing a material concerning a bidirectional reflectance distribution function (brdf) of a specific spectrum.
class Material m s where
    -- | The brdf of this material.
    brdf :: m
         -> Vector Double -- ^ The uvw coordinate to get the brdf at
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


type TextureMap s = Array B Ix2 s

data DiffuseTexture s = DiffuseTexture (TextureMap s) (Vector Double -> (Double, Double))

instance (Show s) => Show (DiffuseTexture s) where
    show (DiffuseTexture tmap _) = "DiffuseTexture (" ++ show tmap ++ ")"

instance (Spectrum s) => Material (DiffuseTexture s) s where
    brdf (DiffuseTexture imgMap proj) uvw _ _ = evaluate' imgMap (r :. c)
        where
            Sz (rows :. columns) = size imgMap
            (u, v) = proj uvw
            rem' x y = x - y * fromIntegral (floor (x/y) :: Int)
            c = floor $ fromIntegral columns * (u `rem'` 1)
            r = rows - 1 - floor (fromIntegral rows * (v `rem'` 1))

readTextureMap :: (Spectrum s) => FilePath -> IO (TextureMap s)
readTextureMap path = do
    img <- MIO.readImageAuto path :: IO Image
    return $ computeAs B $ A.map fromPixel img


data ProceduralDiffuseTexture s = ProceduralDiffuseTexture (Vector Double -> s)

instance Show (ProceduralDiffuseTexture s) where
    show _ = "ProceduralDiffuseTexture"

instance (Spectrum s) => Material (ProceduralDiffuseTexture s) s where
    brdf (ProceduralDiffuseTexture toReflectance) uvw _ _ = toReflectance uvw
