module Scene
    ( Scene (..)
    ) where

import Shape

-- | A type representing a scene of shapes.
data Scene = Scene [Shape]

instance ShapeClass Scene where
    intersect ray (Scene shapes) = intersect ray shapes


