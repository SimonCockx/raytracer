module Vector
    ( Vector
    , Point
    , createVector
    , createPoint
    , normSqr
    , norm
    , Vector.map
    , normalize
    , invert
    , (<+>)
    , (<->)
    , (.*>)
    , (<.>)
    , cross
    ) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric
import Data.Massiv.Core.Operations
import Prelude hiding (map)

-- | A type that represents a vector.
type Vector = Array U Ix1 Double
-- | A type that represents a point.
type Point = Array U Ix1 Double

-- | Create a vector given three coordinates.
createVector :: Double -- ^ The x coordinate
             -> Double -- ^ The y coordinate
             -> Double -- ^ The z coordinate
             -> Vector -- ^ A vector with given coordinates
createVector x y z = fromList Par [x, y, z, 0]

-- | Create a point given three coordinates.
createPoint :: Double -- ^ The x coordinate
            -> Double -- ^ The y coordinate
            -> Double -- ^ The z coordinate
            -> Vector -- ^ A point with given coordinates
createPoint x y z = fromList Par [x, y, z, 1]

-- | Calculate the squared norm of a given vector.
normSqr :: Vector -- ^ The vector to calculate the squared norm of
        -> Double -- ^ The squared norm
normSqr v = v <.> v

-- | Calculate the norm of a given vector.
norm :: Vector -- ^ The vector to calculate the norm of
     -> Double -- ^ The norm
norm = sqrt . normSqr

vmap :: (Double -> Double) -> Vector -> Vector
vmap f v = computeAs U $ A.map f v

-- | Map a function over a vector.
map :: (Double -> Double) -- ^ The function to map
    -> Vector             -- ^ The vector to map the function over
    -> Vector             -- ^ The resulting vector
map f v = computeAs U $ A.imap (\i x -> if i == 3 then 0 else f x) v

-- | Normalize the given vector.
--   When the given vector is the zero vector, it will return the zero vector.
normalize :: Vector -- ^ The given vector to normalize
          -> Vector -- ^ The normalized vector
normalize v
    | n == 0    = v
    | otherwise = vmap (/n) v
    where
        n = norm v

-- | Invert a given vector.
invert :: Vector -- ^ The vector to invert
       -> Vector -- ^ The inverted vector
invert = ((-1) .*>)

infixl 7  .*>, <.>
infixl 6  <+>, <->

-- | Add a point and a vector.
(<+>) :: Point  -- ^ The original point
      -> Vector -- ^ The vector to add
      -> Point  -- ^ The resulting point
p <+> v = computeAs U $ A.zipWith (+) p v

-- | Substract two points.
(<->) :: Point  -- ^ The left-hand side
      -> Point  -- ^ The right-hand side
      -> Vector -- ^ The resulting vector
p1 <-> p2 = computeAs U $ A.zipWith (-) p1 p2

-- | Perform scalar multiplication.
(.*>) :: Double -- ^ The scalar
      -> Vector -- ^ The vector to scale
      -> Vector -- ^ The scaled vector
s .*> v = vmap (*s) v

-- | Perform a dot product.
(<.>) :: Vector -- ^ The left-hand side
      -> Vector -- ^ The right-hand side
      -> Double -- ^ The resulting dot product
v1 <.> v2 = A.sum (A.zipWith (*) v1 v2)

-- | Perform a cross product.
cross :: Vector -- ^ The left-hand side
      -> Vector -- ^ The right-hand side
      -> Vector -- ^ The resulting cross product
v1 `cross` v2 = createVector (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
    where
        x1 = v1 ! 0
        y1 = v1 ! 1
        z1 = v1 ! 2
        x2 = v2 ! 0
        y2 = v2 ! 1
        z2 = v2 ! 2
