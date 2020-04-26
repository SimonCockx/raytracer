{-# LANGUAGE TypeFamilies, UndecidableInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module RayTracer.Lightning.Spectrum
    ( Pixel
    , Image
    , SpectralImage
    , Spectrum (..)
    , toPixel
    , Gray (..)
    , RGB (..)
    , ContinuousSpectrum (..)
    , toImage
    , gammaCorrect
    , inverseGammaCorrect
    , gammaCorrectImage
    , inverseGammaCorrectImage
    , rgb
    , gray
    , zeroV, (^+^), (^-^), (*^), (^*), (^/), sumV
    , averageV
    ) where

import qualified Data.Massiv.Array.IO as MIO
import Data.Massiv.Array as A
import qualified Graphics.ColorSpace as C
import Data.VectorSpace

type Pixel = C.Pixel C.RGB C.Word8
type Image = MIO.Image S C.RGB C.Word8
type SpectralImage spec = A.Array D Ix2 spec

class (VectorSpace a, Double ~ Scalar a, Eq a) => Spectrum a where
    fromPixel :: Pixel -> a
    (^*^) :: a -> a -> a
    white :: a
    black :: a
    black = zeroV
    toRGB :: a -> RGB

toPixel :: (Spectrum s) => s -> Pixel
toPixel spec = let RGB r g b = toRGB spec in rgb r g b

averageV :: (Spectrum s) => [s] -> s
averageV [] = zeroV
averageV spectra = sumSpec^/count
    where
        (sumSpec, count) = foldr (\spec (sSpec, c) -> (sSpec ^+^ spec, c+1)) (zeroV, 0::Double) spectra

toImage :: (Spectrum s) => SpectralImage s -> Image
toImage = (computeAs S) . (A.map toPixel)

rgb :: Double -> Double -> Double -> Pixel
rgb r g b = C.PixelRGB (toWord r) (toWord g) (toWord b)

gray :: Double -> Pixel
gray x = rgb x x x

toWord :: Double -> C.Word8
toWord = (fromIntegral :: Int -> C.Word8) . round . (255*) . min 1 . max 0


gamma :: Double
gamma = 2.2

gammaCorrect :: RGB -> RGB
gammaCorrect (RGB r g b) = RGB (r**(1/gamma)) (g**(1/gamma)) (b**(1/gamma))

inverseGammaCorrect :: RGB -> RGB
inverseGammaCorrect (RGB r g b) = RGB (r**gamma) (g**gamma) (b**gamma)

gammaCorrectImage :: SpectralImage RGB -> SpectralImage RGB
gammaCorrectImage = A.map gammaCorrect

inverseGammaCorrectImage :: SpectralImage RGB -> SpectralImage RGB
inverseGammaCorrectImage = A.map inverseGammaCorrect


newtype Gray = Gray Double
    deriving (Show, Eq)
deriving instance Num Gray
deriving instance AdditiveGroup Gray
deriving instance VectorSpace Gray
instance Spectrum Gray where
    fromPixel (C.PixelRGB r g b) = Gray $ (fromIntegral r + fromIntegral g + fromIntegral b)/3/255
    white = Gray 1.0
    (^*^) = (*)
    toRGB (Gray x) = RGB x x x


data RGB = RGB Double Double Double
    deriving (Show, Eq)
instance AdditiveGroup RGB where
    zeroV = RGB 0.0 0.0 0.0
    (RGB r1 g1 b1) ^+^ (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)
    negateV (RGB r g b) = RGB (-r) (-g) (-b)
    (RGB r1 g1 b1) ^-^ (RGB r2 g2 b2) = RGB (r1 - r2) (g1 - g2) (b1 - b2)
instance VectorSpace RGB where
    type Scalar RGB = Double
    a *^ (RGB r g b) = RGB (a*r) (a*g) (a*b)
instance Spectrum RGB where
    fromPixel (C.PixelRGB r g b) = RGB (fromIntegral r /255) (fromIntegral g /255) (fromIntegral b /255)
    white = RGB 1.0 1.0 1.0
    (RGB r1 g1 b1) ^*^ (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)
    toRGB = id


data ContinuousSpectrum = ZeroContinuousSpectrum
                        | ContinuousSpectrum (Double -> Double)
instance Show ContinuousSpectrum where
    show ZeroContinuousSpectrum = "ZeroContinuousSpectrum"
    show (ContinuousSpectrum _) = "ContinuousSpectrum"
instance Eq ContinuousSpectrum where
    ZeroContinuousSpectrum == ZeroContinuousSpectrum = True
    _ == _ = False
instance AdditiveGroup ContinuousSpectrum where
    zeroV = ZeroContinuousSpectrum

    ZeroContinuousSpectrum ^+^ spec = spec
    spec ^+^ ZeroContinuousSpectrum = spec
    (ContinuousSpectrum i1) ^+^ (ContinuousSpectrum i2) = ContinuousSpectrum (\l -> i1 l + i2 l)

    negateV ZeroContinuousSpectrum = ZeroContinuousSpectrum
    negateV (ContinuousSpectrum i) = ContinuousSpectrum (\l -> negate $ i l)

    ZeroContinuousSpectrum ^-^ spec = negateV spec
    spec ^-^ ZeroContinuousSpectrum = spec
    (ContinuousSpectrum i1) ^-^ (ContinuousSpectrum i2) = ContinuousSpectrum (\l -> i1 l - i2 l)
instance VectorSpace ContinuousSpectrum where
    type Scalar ContinuousSpectrum = Double
    _ *^ ZeroContinuousSpectrum = ZeroContinuousSpectrum
    0 *^ _ = ZeroContinuousSpectrum
    a *^ (ContinuousSpectrum i) = ContinuousSpectrum (\l -> a * i l)
instance Spectrum ContinuousSpectrum where
    fromPixel _ = error "Not implemented"
    white = error "Not implemented"
    ZeroContinuousSpectrum ^*^ _ = ZeroContinuousSpectrum
    _ ^*^ ZeroContinuousSpectrum = ZeroContinuousSpectrum
    (ContinuousSpectrum i1) ^*^ (ContinuousSpectrum i2) = ContinuousSpectrum (\l -> i1 l * i2 l)
    toRGB _ = error "Not implemented"

