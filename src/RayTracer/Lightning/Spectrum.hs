{-# LANGUAGE TypeFamilies, UndecidableInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module RayTracer.Lightning.Spectrum
    ( Pixel
    , Image
    , SpectralImage
    , Spectrum (..)
    , Gray (..)
    , RGB (..)
    , toImage
    , gammaCorrect
    , gammaCorrectImage
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
    toPixel :: a -> Pixel
    fromPixel :: Pixel -> a
    (^*^) :: a -> a -> a
    white :: a
    black :: a
    black = zeroV
    smap :: (Double -> Double) -> a -> a
    toRGB :: a -> RGB

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

gammaCorrect :: (Spectrum s) => s -> s
gammaCorrect = smap (**(1/gamma))

gammaCorrectImage :: (Spectrum s) => SpectralImage s -> SpectralImage s
gammaCorrectImage = A.map gammaCorrect


newtype Gray = Gray Double
    deriving (Show, Eq)
deriving instance Num Gray
deriving instance AdditiveGroup Gray
instance VectorSpace Gray where
    type Scalar Gray = Double
    (*^) a = smap (a*)
instance Spectrum Gray where
    toPixel (Gray x) = gray x
    fromPixel (C.PixelRGB r g b) = Gray $ (fromIntegral r + fromIntegral g + fromIntegral b)/3/255
    white = Gray 1.0
    (^*^) = (*)
    smap f (Gray x) = Gray (f x)
    toRGB (Gray x) = RGB x x x


data RGB = RGB Double Double Double
    deriving (Show, Eq)
instance AdditiveGroup RGB where
    zeroV = RGB 0.0 0.0 0.0
    (RGB r1 g1 b1) ^+^ (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)
    negateV = smap negate
    (RGB r1 g1 b1) ^-^ (RGB r2 g2 b2) = RGB (r1 - r2) (g1 - g2) (b1 - b2)
instance VectorSpace RGB where
    type Scalar RGB = Double
    (*^) a = smap (a*)
instance Spectrum RGB where
    toPixel (RGB r g b) = rgb r g b
    fromPixel (C.PixelRGB r g b) = RGB (fromIntegral r /255) (fromIntegral g /255) (fromIntegral b /255)
    white = RGB 1.0 1.0 1.0
    (RGB r1 g1 b1) ^*^ (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)
    smap f (RGB r g b) = RGB (f r) (f g) (f b)
    toRGB = id
