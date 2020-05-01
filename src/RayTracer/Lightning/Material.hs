module RayTracer.Lightning.Material
    ( BRDF
    , ReflectanceSample
    , InspectingHit (..)
    , ReflectingHit (..)
    , ShadowHit (..)
    , Material (..)
    , MaterialHit
    , inspectHit
    , reflectHit
    , shadowReflectHit
    , hitMaterial
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
    , Reflective (..)
    ) where

import RayTracer.Geometry
import RayTracer.Lightning.Spectrum
import RayTracer.Lightning.Light
import RayTracer.Core.Sampling
import Data.Massiv.Array hiding (mapM, map)
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as MIO
import RayTracer.Random

import Data.Maybe

type ReflectanceSample s = (Point Double, s)

type BRDF s = Vector Double -> Vector Double -> s
-- emit, brdf, shading intersection
data InspectingHit s = InspectingHit s (BRDF s) Intersection
instance (Show s) => Show (InspectingHit s) where
    show (InspectingHit emit _ int) = "InspectingHit (" ++ show emit ++ ") BRDF " ++ show int
-- emit, reflectance, new ray
data ReflectingHit s = ReflectingHit s s (Ray Double)
    deriving (Show)
-- emit, [reflectance*incomming radiance sample]
data ShadowHit s = ShadowHit s [ReflectanceSample s]
    deriving (Show)

class Material a s where
    inspect :: a -> Ray Double -> Intersection -> InspectingHit s
    reflect :: (MonadRandom m) => a -> Ray Double -> Intersection -> m (ReflectingHit s)
    default reflect :: (MonadRandom m, Spectrum s) => a -> Ray Double -> Intersection -> m (ReflectingHit s)
    reflect mat ray intersection = do
        phi <- (*(2*pi)) <$> getRandom
        cosTheta <- sqrt <$> getRandomR (0, 1)
        let InspectingHit emit brdf (t, normal, _) = inspect mat ray intersection
            theta = acos cosTheta
            newDirec = alignWithXAxis normal `inverseTransform` rotateX phi `transform` rotateZ theta `transform` Vector 1 0 0
            newRay = Ray (shadowPoint (follow ray t) normal) newDirec
        return $ ReflectingHit emit (pi *^ brdf newDirec (direction ray)) newRay
    shadowReflect :: (MonadRandom m) => a -> Ray Double -> Intersection -> SamplingStrategy -> [Light s] -> m (ShadowHit s)
    default shadowReflect :: (MonadRandom m, Spectrum s, Show s) => a -> Ray Double -> Intersection -> SamplingStrategy -> [Light s] -> m (ShadowHit s)
    shadowReflect mat ray intersection strat lights = ShadowHit emit . concat <$> samplesPerLight
        where
            InspectingHit emit brdf (t, normal, _) = inspect mat ray intersection
            p = shadowPoint (follow ray t) normal
            l_out = negateV $ direction ray
            reflSample (pl, spec) = (pl, reflectedSpec)
                where
                    l_in = if p == pl then normal else normalize $ pl <-> p
                    reflectedSpec = max 0 (l_in <.> normal) *^ brdf l_in l_out ^*^ spec
            samplesPerLight = (`mapM` lights) $ \light -> do
                samples <- generateSample strat light p
                return $ map reflSample samples


instance (Spectrum s1, s1 ~ s2, Show s1) => Material (Light s1) s2 where
    inspect l ray intersection@(t, _, _) = InspectingHit (getRadiance l (follow ray t) (origin ray)) blackBRDF intersection


data MaterialHit s = forall a. (Material a s) => MaterialHit a (Ray Double) Intersection 
inspectHit :: MaterialHit s -> InspectingHit s
inspectHit (MaterialHit mat ray int) = inspect mat ray int
reflectHit :: (MonadRandom m) => MaterialHit s -> m (ReflectingHit s)
reflectHit (MaterialHit mat ray int) = reflect mat ray int
shadowReflectHit :: (MonadRandom m) => MaterialHit s -> SamplingStrategy -> [Light s] -> m (ShadowHit s)
shadowReflectHit (MaterialHit mat ray int) = shadowReflect mat ray int

hitMaterial :: (Material a s) => a -> Ray Double -> Intersection -> MaterialHit s
hitMaterial = MaterialHit

instance (Material a s) => Material (Transformed a) s where
    inspect (Transformed tr mat) ray (t, n, uvw) = 
        let InspectingHit emit brdf (t', n', uvw') = inspect mat (inverseTransform tr ray) (t, inverseNormalTransform tr n, uvw)
        in
            InspectingHit emit (\w1 w2 -> brdf (inverseTransform tr w1) (inverseTransform tr w2)) (t', normalTransform tr n', uvw')
    reflect (Transformed tr mat) ray (t, n, uvw) = do
        ReflectingHit emit refl newRay <- reflect mat (inverseTransform tr ray) (t, inverseNormalTransform tr n, uvw)
        return $ ReflectingHit emit refl (transform tr newRay)
    shadowReflect = error "Not implemented. (see TODO.md)"
        

instance Transformable (MaterialHit s) Double where
    transform tr (MaterialHit mat ray int) = MaterialHit (Transformed tr mat) ray int


uniform :: (Spectrum s) => BRDF s -> Intersection -> InspectingHit s
uniform = InspectingHit black

diffuseBRDF :: (Spectrum s) => s -> BRDF s
diffuseBRDF reflectance _ _ = (1/pi) *^ reflectance

newtype Diffuse s = Diffuse s
    deriving (Show)
instance (Spectrum s1, s1 ~ s2, Show s1) => Material (Diffuse s1) s2 where
    inspect (Diffuse reflectance) _ = uniform $ diffuseBRDF reflectance


whiteBRDF :: (Spectrum s) => BRDF s
whiteBRDF = diffuseBRDF white

whiteMaterial :: (Spectrum s) => Diffuse s
whiteMaterial = Diffuse white

blackBRDF :: (Spectrum s) => BRDF s
blackBRDF = diffuseBRDF black

blackMaterial :: (Spectrum s) => Diffuse s
blackMaterial = Diffuse black


type TextureMap s = Array B Ix2 s

data DiffuseTexture s = DiffuseTexture (TextureMap s) (Vector Double -> (Double, Double))
instance Show (DiffuseTexture s) where
    show _ = "DiffuseTexture"
instance (Spectrum s1, s1 ~ s2, Show s1) => Material (DiffuseTexture s1) s2 where
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

newtype ProceduralDiffuseTexture s = ProceduralDiffuseTexture (Vector Double -> s)
instance Show (ProceduralDiffuseTexture s) where
    show _ = "ProceduralDiffuseTexture"
instance (Spectrum s1, s1 ~ s2, Show s1) => Material (ProceduralDiffuseTexture s1) s2 where
    inspect (ProceduralDiffuseTexture toReflectance) _ intersection@(_, _, uvw) = uniform (diffuseBRDF $ toReflectance uvw) intersection


data Reflective = Reflective
    deriving (Show)
instance (Spectrum s, Show s) => Material Reflective s where
    inspect Reflective _ = InspectingHit black blackBRDF
    reflect Reflective ray intersection = 
        let InspectingHit emit _ (t, normal, _) = inspect Reflective ray intersection
            direc = direction ray
            newDirec = direc ^-^ (2 * direc <.> normal) *^ normal
            newRay = Ray (shadowPoint (follow ray t) normal) newDirec
        in
            return $ ReflectingHit emit white newRay
    shadowReflect Reflective ray int _ lights = uncurry ShadowHit <$> do
        ReflectingHit emit refl newRay <- reflect Reflective ray int
        let samplesPerLight = (`map` lights) $ \light -> do 
                (t, _, _) <- intersect newRay light
                let pl = follow newRay t
                return (pl, refl ^*^ getRadiance light pl (origin newRay))
        return (emit, catMaybes samplesPerLight)
