module RayTracer.Geometry.Transformation
    ( Transformation
    , Transformable (..)
    , inverseTransform
    , Transformed (..)
    , innerTransform
    , inverse
    , normalTransform
    , inverseNormalTransform
    , identity
    , translate
    , translateP
    , translateV
    , rotateX
    , rotateY
    , rotateZ
    , rotateAround
    , scale
    , scaleUni
    , alignWithXAxis
    ) where

import RayTracer.Geometry.Vector

data Matrix a = Matrix !a !a !a !a
                       !a !a !a !a
                       !a !a !a !a
                       !a !a !a !a
    deriving (Show)

transpose :: Matrix a -> Matrix a
transpose (Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44) =
    Matrix m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44

transformMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
transformMatrix (Matrix l11 l12 l13 l14 l21 l22 l23 l24 l31 l32 l33 l34 l41 l42 l43 l44) (Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44) =
    Matrix n11 n12 n13 n14 n21 n22 n23 n24 n31 n32 n33 n34 n41 n42 n43 n44
    where
        n11 = l11 * m11 + l12 * m21 + l13 * m31 + l14 * m41
        n12 = l11 * m12 + l12 * m22 + l13 * m32 + l14 * m42
        n13 = l11 * m13 + l12 * m23 + l13 * m33 + l14 * m43
        n14 = l11 * m14 + l12 * m24 + l13 * m34 + l14 * m44
        n21 = l21 * m11 + l22 * m21 + l23 * m31 + l24 * m41
        n22 = l21 * m12 + l22 * m22 + l23 * m32 + l24 * m42
        n23 = l21 * m13 + l22 * m23 + l23 * m33 + l24 * m43
        n24 = l21 * m14 + l22 * m24 + l23 * m34 + l24 * m44
        n31 = l31 * m11 + l32 * m21 + l33 * m31 + l34 * m41
        n32 = l31 * m12 + l32 * m22 + l33 * m32 + l34 * m42
        n33 = l31 * m13 + l32 * m23 + l33 * m33 + l34 * m43
        n34 = l31 * m14 + l32 * m24 + l33 * m34 + l34 * m44
        n41 = l41 * m11 + l42 * m21 + l43 * m31 + l44 * m41
        n42 = l41 * m12 + l42 * m22 + l43 * m32 + l44 * m42
        n43 = l41 * m13 + l42 * m23 + l43 * m33 + l44 * m43
        n44 = l41 * m14 + l42 * m24 + l43 * m34 + l44 * m44

transformVector :: (Num a) => Matrix a -> Vector a -> Vector a
transformVector (Matrix m11 m12 m13 _ m21 m22 m23 _ m31 m32 m33 _ _ _ _ _) (Vector x y z) = Vector x' y' z'
    where
        x' = m11*x + m12*y + m13*z
        y' = m21*x + m22*y + m23*z
        z' = m31*x + m32*y + m33*z

transformPoint :: (Num a) => Matrix a -> Point a -> Point a
transformPoint (Matrix m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 _ _ _ _) (Point x y z) = Point x' y' z'
    where
        x' = m11*x + m12*y + m13*z + m14
        y' = m21*x + m22*y + m23*z + m24
        z' = m31*x + m32*y + m33*z + m34

-- | A type that represents a transformation.
data Transformation a = Transformation !(Matrix a) !(Matrix a)
    deriving (Show)

class Transformable v a where
    -- | Transform a transformable with the given transformation.
    transform :: Transformation a -- ^ The transformation
              -> v                -- ^ The transformable to be transformed
              -> v                -- ^ The transformed transformable

-- | Inversely transform a transformable with the given transformation.
inverseTransform :: (Transformable v a)
                 => Transformation a -- ^ The transformation
                 -> v                -- ^ The transformable to be inversely transformed
                 -> v                -- ^ The inversely transformed transformable
inverseTransform t = transform $ inverse t


instance (Num a1, a1 ~ a2) => Transformable (Matrix a1) a2 where
    transform (Transformation mat _) = transformMatrix mat
instance (Num a1, a1 ~ a2) => Transformable (Transformation a1) a2 where
    transform (Transformation mat1 inv1) (Transformation mat2 inv2) = Transformation mat inv
        where
            mat = transformMatrix mat1 mat2
            inv = transformMatrix inv2 inv1
instance (Num a1, a1 ~ a2) => Transformable (Vector a1) a2 where
    transform (Transformation mat _) = transformVector mat
instance (Num a1, a1 ~ a2) => Transformable (Point a1) a2 where
    transform (Transformation mat _) = transformPoint mat
instance (Transformable t a) => Transformable [t] a where
    transform trans = map (transform trans)


data Transformed a = Transformed !(Transformation Double) !a
    deriving (Show)
instance Transformable (Transformed a) Double where
    transform t (Transformed t' s) = Transformed (t `transform` t') s

innerTransform :: Transformation Double -> Transformed a -> Transformed a
innerTransform tr (Transformed tr' s) = Transformed (tr' `transform` tr) s


-- | Invert a transformation.
inverse :: Transformation a -- ^ The transformation to invert
        -> Transformation a -- ^ The inverted transformation
inverse (Transformation mat inv) = Transformation inv mat

-- | Transform a normal with the given transformation and normalize the result.
normalTransform :: (Floating a, Eq a, AdditiveGroup a)
                => Transformation a -- ^ The transformation
                -> Vector a         -- ^ The normal to be transformed
                -> Vector a         -- ^ The transformed normal
normalTransform (Transformation _ inv) = normalize . transformVector (transpose inv)
inverseNormalTransform :: (Floating a, Eq a, AdditiveGroup a)
                       => Transformation a -- ^ The transformation
                       -> Vector a         -- ^ The normal to be transformed inversely
                       -> Vector a         -- ^ The inversely transformed normal
inverseNormalTransform t = normalTransform $ inverse t


identity :: (Num a) => Transformation a
identity = Transformation i i
    where
        i = Matrix 1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   0 0 0 1
{-# INLINE identity #-}

-- | Create a translation with given coordinates.
translate :: (Num a)
          => a                -- ^ The x translation
          -> a                -- ^ The y translation
          -> a                -- ^ The z translation
          -> Transformation a -- ^ The resulting translation
translate x y z = Transformation mat inv
    where
        mat = Matrix 1 0 0 x
                     0 1 0 y
                     0 0 1 z
                     0 0 0 1
        inv = Matrix 1 0 0 (-x)
                     0 1 0 (-y)
                     0 0 1 (-z)
                     0 0 0 1
{-# INLINE translate #-}

translateP :: (Num a)
           => Point a
           -> Transformation a
translateP (Point x y z) = translate x y z
{-# INLINE translateP #-}

translateV :: (Num a)
           => Vector a
           -> Transformation a
translateV (Vector x y z) = translate x y z
{-# INLINE translateV #-}

-- | Create a rotation around the x-axis with given angle in radians.
rotateX :: (Floating a)
        => a                -- ^ The angle in radians
        -> Transformation a -- ^ The resulting rotation
rotateX angle = Transformation mat inv
    where
        c = cos angle
        s = sin angle
        ns = -s
        mat = Matrix 1 0  0 0
                     0 c ns 0
                     0 s  c 0
                     0 0  0 1
        inv = transpose mat
{-# INLINE rotateX #-}

-- | Create a rotation around the y-axis with given angle in radians.
rotateY :: (Floating a)
        => a                -- ^ The angle in radians
        -> Transformation a -- ^ The resulting rotation
rotateY angle = Transformation mat inv
    where
        c = cos angle
        s = sin angle
        ns = -s
        mat = Matrix c  0 s 0
                     0  1 0 0
                     ns 0 c 0
                     0  0 0 1
        inv = transpose mat
{-# INLINE rotateY #-}

-- | Create a rotation around the z-axis with given angle in radians.
rotateZ :: (Floating a)
        => a                -- ^ The angle in radians
        -> Transformation a -- ^ The resulting rotation
rotateZ angle = Transformation mat inv
    where
        c = cos angle
        s = sin angle
        ns = -s
        mat = Matrix c ns 0 0
                     s c  0 0
                     0 0  1 0
                     0 0  0 1
        inv = transpose mat
{-# INLINE rotateZ #-}


rotateAround :: (RealFloat a, AdditiveGroup a) => Vector a -> a -> Transformation a
rotateAround n angle = align `inverseTransform` rotateX angle `transform` align
    where
        align = alignWithXAxis n
{-# INLINE rotateAround #-}


-- | Create a scaling transformation with given factors.
scale :: (Fractional a)
      => a                -- ^ The x factor
      -> a                -- ^ The y factor
      -> a                -- ^ The z factor
      -> Transformation a -- ^ The resulting scaling transformation
scale xf yf zf = Transformation mat inv
    where
        mat = Matrix xf 0  0  0
                     0  yf 0  0
                     0  0  zf 0
                     0  0  0  1
        inv = Matrix (1/xf) 0    0    0
                       0  (1/yf) 0    0
                       0    0  (1/zf) 0
                       0    0    0    1
{-# INLINE scale #-}

-- | Create a uniform scaling transformation with given factor.
scaleUni :: (Fractional a)
      => a                -- ^ The factor
      -> Transformation a -- ^ The resulting scaling transformation
scaleUni f = scale f f f
{-# INLINE scaleUni #-}


alignWithXAxis :: (RealFloat a, AdditiveGroup a) => Vector a -> Transformation a
alignWithXAxis v@(Vector x y z) =
    let phi = atan2 y x
        theta = asin $ z/l
    in rotateY theta `transform` rotateZ (-phi)
    where
        l = norm v
{-# INLINE alignWithXAxis #-}
