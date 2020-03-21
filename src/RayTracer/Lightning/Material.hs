module RayTracer.Lightning.Material
    ( BRDF
    , Material
    , diffuseBRDF
    , diffuse
    , whiteBRDF
    , whiteMaterial
    , blackBRDF
    , blackMaterial
    ) where

import RayTracer.Geometry
import RayTracer.Lightning.Spectrum

type BRDF s = Vector Double -> Vector Double -> s
type Material s = Vector Double -> BRDF s

uniform :: BRDF s -> Material s
uniform brdf _ = brdf

diffuseBRDF :: s -> BRDF s
diffuseBRDF reflectance _ _ = reflectance

diffuse :: s -> Material s
diffuse reflectance = uniform $ diffuseBRDF reflectance

whiteBRDF :: (Spectrum s) => BRDF s
whiteBRDF = diffuseBRDF white

whiteMaterial :: (Spectrum s) => Material s
whiteMaterial = uniform whiteBRDF

blackBRDF :: (Spectrum s) => BRDF s
blackBRDF = diffuseBRDF black

blackMaterial :: (Spectrum s) => Material s
blackMaterial = uniform blackBRDF
