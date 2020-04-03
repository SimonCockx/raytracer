module RayTracer.Lightning.Material
    ( BRDF
    , Hit (..)
    , Material (..)
    , uniform
    , diffuseBRDF
    , Diffuse (..)
    , whiteBRDF
    , whiteMaterial
    , blackBRDF
    , blackMaterial
    , TextureMap
    , DiffuseTexture (..)
    , readTextureMap
    , ProceduralDiffuseTexture (..)
    ) where

import RayTracer.Geometry
import RayTracer.Lightning.Spectrum
import Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as MIO


type BRDF s = Vector Double -> Vector Double -> s
data Hit s = Hit s (BRDF s) Intersection

class Material m s where
    inspect :: m -> Ray Double -> Intersection -> Hit s

uniform :: (Spectrum s) => BRDF s -> Intersection -> Hit s
uniform = Hit zeroV

diffuseBRDF :: s -> BRDF s
diffuseBRDF reflectance _ _ = reflectance

newtype Diffuse s = Diffuse s
    deriving (Show)
instance (Spectrum s1, s1 ~ s2) => Material (Diffuse s1) s2 where
    inspect (Diffuse reflectance) _ = uniform $ diffuseBRDF reflectance

whiteBRDF :: (Spectrum s) => BRDF s
whiteBRDF = diffuseBRDF white

-- whiteMaterial :: (Spectrum s) => Material s
-- whiteMaterial = uniform whiteBRDF
whiteMaterial :: (Spectrum s) => Diffuse s
whiteMaterial = Diffuse white

blackBRDF :: (Spectrum s) => BRDF s
blackBRDF = diffuseBRDF black

-- blackMaterial :: (Spectrum s) => Material s
-- blackMaterial = uniform blackBRDF
blackMaterial :: (Spectrum s) => Diffuse s
blackMaterial = Diffuse black


type TextureMap s = Array B Ix2 s

data DiffuseTexture s = DiffuseTexture (TextureMap s) (Vector Double -> (Double, Double))
instance Show (DiffuseTexture s) where
    show _ = "DiffuseTexture"
instance (Spectrum s1, s1 ~ s2) => Material (DiffuseTexture s1) s2 where
    inspect (DiffuseTexture textureMap proj) _ intersection@(_, _, uvw) = uniform (diffuseBRDF $ evaluate' textureMap (r :. c)) intersection
        where
            Sz (rows :. columns) = size textureMap
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
instance (Spectrum s1, s1 ~ s2) => Material (ProceduralDiffuseTexture s1) s2 where
    inspect (ProceduralDiffuseTexture toReflectance) _ intersection@(_, _, uvw) = uniform (diffuseBRDF $ toReflectance uvw) intersection

-- proceduralDiffuseTexture :: (Spectrum s) => (Vector Double -> s) -> Material s
-- proceduralDiffuseTexture toReflectance intersection@(_, _, uvw) = (zeroV, diffuseBRDF $ toReflectance uvw, intersection)
