module RayTracer.Geometry.Vector
    ( Vector (..)
    , Point (..)
    , toVector
    , toPoint
    , cosAngle
    , normSqr
    , norm
    , normalize
    , (<+^)
    , (<->)
    , cross
    , zeroV, negateV, (^+^), (^-^), (*^), (^*), (^/), (<.>), sumV, AdditiveGroup
    ) where

import Data.VectorSpace hiding ((<.>))
import qualified Data.VectorSpace as V ((<.>))

-- | A type that represents a vector.
data Vector a = Vector a a a
    deriving (Eq, Show)
-- | A type that represents a point.
data Point a = Point a a a
    deriving (Eq, Show)


instance Functor Vector where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)
    {-# INLINE fmap #-}
instance Applicative Vector where
    pure x = Vector x x x
    {-# INLINE pure #-}
    (Vector fx fy fz) <*> (Vector x y z) = Vector (fx x) (fy y) (fz z)
    {-# INLINE (<*>) #-}
instance Foldable Vector where
    foldr f acc (Vector x y z) = x `f` (y `f` (z `f` acc))
    {-# INLINE foldr #-}
instance Traversable Vector where
    traverse f (Vector x y z) = fromList <$> traverse f [x, y, z]
        where
            fromList [a, b, c] = Vector a b c -- TODO: kan dit beter?
            fromList _ = error "Not implemented..."
            {-# INLINE fromList #-}
    {-# INLINE traverse #-}
instance (Num a) => AdditiveGroup (Vector a) where
    zeroV = Vector 0 0 0
    {-# INLINE zeroV #-}
    v1 ^+^ v2 = (+) <$> v1 <*> v2
    {-# INLINE (^+^) #-}
    negateV = fmap negate
    {-# INLINE negateV #-}
    v1 ^-^ v2 = (-) <$> v1 <*> v2
    {-# INLINE (^-^) #-}
instance (Num a) => VectorSpace (Vector a) where
    type Scalar (Vector a) = a
    a *^ v = fmap (a*) v
    {-# INLINE (*^) #-}
instance (Num a, AdditiveGroup a) => InnerSpace (Vector a) where
    v1 <.> v2 = sum $ (*) <$> v1 <*> v2
    {-# INLINE (<.>) #-}

infixr 8 <.>
(<.>) :: (InnerSpace v) => v -> v -> Scalar v
(<.>) = (V.<.>)
{-# INLINE (<.>) #-}


instance Functor Point where
    fmap f (Point x y z) = Point (f x) (f y) (f z)
    {-# INLINE fmap #-}
instance Applicative Point where
    pure x = Point x x x
    {-# INLINE pure #-}
    (Point fx fy fz) <*> (Point x y z) = Point (fx x) (fy y) (fz z)
    {-# INLINE (<*>) #-}
instance Foldable Point where
    foldr f acc (Point x y z) = x `f` (y `f` (z `f` acc))
    {-# INLINE foldr #-}
instance Traversable Point where
    traverse f (Point x y z) = fromList <$> traverse f [x, y, z]
        where
            fromList [a, b, c] = Point a b c -- TODO: kan dit beter?
            fromList _ = error "Not implemented..."
            {-# INLINE fromList #-}
    {-# INLINE traverse #-}

toVector :: Point a -> Vector a
toVector (Point x y z) = Vector x y z
{-# INLINE toVector #-}

toPoint :: Vector a -> Point a
toPoint (Vector x y z) = Point x y z
{-# INLINE toPoint #-}

cosAngle :: (Floating a, AdditiveGroup a) => Vector a -> Vector a -> a
cosAngle v1 v2 = (v1 <.> v2) / sqrt (normSqr v1 * normSqr v2)
{-# INLINE cosAngle #-}

-- | Calculate the squared norm of a given vector.
normSqr :: (Num a, AdditiveGroup a)
        => Vector a -- ^ The vector to calculate the squared norm of
        -> a        -- ^ The squared norm
normSqr v = v <.> v
{-# INLINE normSqr #-}

-- | Calculate the norm of a given vector.
norm :: (Floating a, AdditiveGroup a)
     => Vector a -- ^ The vector to calculate the norm of
     -> a        -- ^ The norm
norm = sqrt . normSqr
{-# INLINE norm #-}

-- | Normalize the given vector.
--   When the given vector is the zero vector, it will return the zero vector.
normalize :: (Floating a, Eq a, AdditiveGroup a)
          => Vector a -- ^ The given vector to normalize
          -> Vector a -- ^ The normalized vector
normalize v
    | n == 0    = v
    | otherwise = fmap (/n) v
    where
        n = norm v
{-# INLINE normalize #-}

infixl 6  <+^, <->

-- | Add a point and a vector.
(<+^) :: (Num a)
      => Point a  -- ^ The original point
      -> Vector a -- ^ The vector to add
      -> Point a  -- ^ The resulting point
(Point x1 y1 z1) <+^ (Vector x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)
{-# INLINE (<+^) #-}

-- | Substract two points.
(<->) :: (Num a)
      => Point a  -- ^ The left-hand side
      -> Point a  -- ^ The right-hand side
      -> Vector a -- ^ The resulting vector
(Point x1 y1 z1) <-> (Point x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
{-# INLINE (<->) #-}

-- | Perform a cross product.
cross :: (Num a)
      => Vector a -- ^ The left-hand side
      -> Vector a -- ^ The right-hand side
      -> Vector a -- ^ The resulting cross product
(Vector x1 y1 z1) `cross` (Vector x2 y2 z2) = Vector (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
{-# INLINE cross #-}
