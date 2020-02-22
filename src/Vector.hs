module Vector
    ( Vector (..)
    , Point (..)
    , normSqr
    , norm
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
import Prelude

-- | A type that represents a vector.
data Vector a = Vector a a a
-- | A type that represents a point.
data Point a = Point a a a

instance (Num a) => Num (Vector a) where
      (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)
      (Vector x1 y1 z1) - (Vector x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)
      (Vector x1 y1 z1) * (Vector x2 y2 z2) = Vector (x1*x2) (y1*y2) (z1*z2)
      negate = fmap negate
      abs = fmap abs
      signum = fmap signum
      fromInteger x = pure (fromInteger x)
instance Functor Vector where
      fmap f (Vector x y z) = Vector (f x) (f y) (f z)
instance Applicative Vector where
      pure x = Vector x x x
      (Vector fx fy fz) <*> (Vector x y z) = Vector (fx x) (fy y) (fz z)
instance Functor Point where
      fmap f (Point x y z) = Point (f x) (f y) (f z)
instance Applicative Point where
      pure x = Point x x x
      (Point fx fy fz) <*> (Point x y z) = Point (fx x) (fy y) (fz z)

-- | Calculate the squared norm of a given vector.
normSqr :: (Num a) 
        => Vector a -- ^ The vector to calculate the squared norm of
        -> a        -- ^ The squared norm
normSqr v = v <.> v

-- | Calculate the norm of a given vector.
norm :: (Floating a) 
     => Vector a -- ^ The vector to calculate the norm of
     -> a        -- ^ The norm
norm = sqrt . normSqr

-- | Normalize the given vector.
--   When the given vector is the zero vector, it will return the zero vector.
normalize :: (Floating a, Eq a)
          => Vector a -- ^ The given vector to normalize
          -> Vector a -- ^ The normalized vector
normalize v
    | n == 0    = v
    | otherwise = fmap (/n) v
    where
        n = norm v

-- | Invert a given vector.
invert :: (Num a)
       => Vector a -- ^ The vector to invert
       -> Vector a -- ^ The inverted vector
invert = ((-1) .*>)

infixl 7  .*>, <.>
infixl 6  <+>, <->

-- | Add a point and a vector.
(<+>) :: (Num a)
      => Point a  -- ^ The original point
      -> Vector a -- ^ The vector to add
      -> Point a  -- ^ The resulting point
(Point x1 y1 z1) <+> (Vector x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- | Substract two points.
(<->) :: (Num a)
      => Point a  -- ^ The left-hand side
      -> Point a  -- ^ The right-hand side
      -> Vector a -- ^ The resulting vector
(Point x1 y1 z1) <-> (Point x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

-- | Perform scalar multiplication.
(.*>) :: (Num a)
      => a        -- ^ The scalar
      -> Vector a -- ^ The vector to scale
      -> Vector a -- ^ The scaled vector
s .*> v = fmap (*s) v

-- | Perform a dot product.
(<.>) :: (Num a)
      => Vector a -- ^ The left-hand side
      -> Vector a -- ^ The right-hand side
      -> a        -- ^ The resulting dot product
(Vector x1 y1 z1) <.> (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | Perform a cross product.
cross :: (Num a)
      => Vector a -- ^ The left-hand side
      -> Vector a -- ^ The right-hand side
      -> Vector a -- ^ The resulting cross product
(Vector x1 y1 z1) `cross` (Vector x2 y2 z2) = Vector (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
