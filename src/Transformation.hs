module Transformation
    ( Transformation
    , transform
    , translate
    , rotateX
    , rotateY
    , rotateZ
    , scale
    , (Transformation.|*|)
    , (|*>)
    ) where

import Data.Massiv.Array as A
import qualified Data.Massiv.Array.Numeric as N
import Data.Maybe
import Vector

-- | A type that represents a transformation.
type Transformation = Array U Ix2 Double

-- | Create a transformation with given values.
transform :: Double         -- ^ The value at (1, 1)
          -> Double         -- ^ The value at (1, 2)
          -> Double         -- ^ The value at (1, 3)
          -> Double         -- ^ The value at (1, 4)
          -> Double         -- ^ The value at (2, 1)
          -> Double         -- ^ The value at (2, 2)
          -> Double         -- ^ The value at (2, 3)
          -> Double         -- ^ The value at (2, 4)
          -> Double         -- ^ The value at (3, 1)
          -> Double         -- ^ The value at (3, 2)
          -> Double         -- ^ The value at (3, 3)
          -> Double         -- ^ The value at (3, 4)
          -> Transformation -- ^ A transformation with given values
transform m11 m12 m13 m14
          m21 m22 m23 m24
          m31 m32 m33 m34 = fromLists' Par [[m11, m12, m13, m14]
                                           ,[m21, m22, m23, m24]
                                           ,[m31, m32, m33, m34]
                                           ,[0,   0,   0,   1  ]]

-- | Create a translation with given coordinates.
translate :: Double         -- ^ The x translation
          -> Double         -- ^ The y translation
          -> Double         -- ^ The z translation
          -> Transformation -- ^ The resulting translation
translate x y z = transform 1 0 0 x
                            0 1 0 y
                            0 0 1 z

-- | Create a rotation around the x-axis with given angle in radians.
rotateX :: Double         -- ^ The angle in radians
        -> Transformation -- ^ The resulting rotation
rotateX angle = transform 1 0  0 0
                          0 c ns 0
                          0 s  c 0
    where
        c = cos angle
        s = sin angle
        ns = -s

-- | Create a rotation around the y-axis with given angle in radians.
rotateY :: Double         -- ^ The angle in radians
        -> Transformation -- ^ The resulting rotation
rotateY angle = transform  c 0 s 0
                           0 1 0 0
                          ns 0 c 0
    where
        c = cos angle
        s = sin angle
        ns = -s

-- | Create a rotation around the z-axis with given angle in radians.
rotateZ :: Double         -- ^ The angle in radians
        -> Transformation -- ^ The resulting rotation
rotateZ angle = transform 1 0  0 0
                          0 c ns 0
                          0 s  c 0
    where
        c = cos angle
        s = sin angle
        ns = -s

-- | Create a scaling transformation with given factors.
scale :: Double         -- ^ The x factor
      -> Double         -- ^ The y factor
      -> Double         -- ^ The z factor
      -> Transformation -- ^ The resulting scaling transformation
scale xf yf zf = transform xf 0  0  0
                           0  yf 0  0
                           0  0  zf 0


-- | Perform a matrix multiplication.
(|*|) :: Transformation -- ^ The left-hand side of the multiplication
      -> Transformation -- ^ The right-hand side of the multiplication
      -> Transformation -- ^ The resulting transformation
t1 |*| t2 = fromJust $ t1 N.|*| t2


-- | Transform a vector with the given transformation.
(|*>) :: Transformation -- ^ The transformation
      -> Vector         -- ^ The vector to be transformed
      -> Vector         -- ^ The transformed vector
t |*> v = resize' (Sz 4) $ fromJust $ t N.|*| (resize' (Sz (4 :. 1)) v)
