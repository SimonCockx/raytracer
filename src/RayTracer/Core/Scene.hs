{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Core.Scene
    ( Scene (..)
    , render
    ) where

import RayTracer.Core.World
import RayTracer.Core.Camera
import RayTracer.Core.RayTracer
import RayTracer.Lightning
import RayTracer.Random

-- | A type representing a scene of shapes.
data Scene s = forall cam. (Camera cam, Show cam) => Scene (World s) cam

instance (Show s) => Show (Scene s) where
    show (Scene world cam) = "Scene (" ++ show world ++ ") (" ++ show cam ++ ")"

render :: (MonadRandom m, MonadSplit g m, MonadRunner g m, RandomGen g, Spectrum s, RayTracer r s) => r -> Scene s -> m Image
render tracer (Scene world camera) = do
    spectralImage <- rayTrace camera tracer world
    return $ toImage spectralImage
