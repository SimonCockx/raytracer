{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Core.Scene
    ( Scene (..)
    , render
    ) where

import System.Random
import RayTracer.Core.World
import RayTracer.Core.Camera
import RayTracer.Core.RayTracer
import RayTracer.Lightning

-- | A type representing a scene of shapes.
data Scene s = forall cam. (Camera cam, Show cam) => Scene (World s) cam

instance (Show s) => Show (Scene s) where
    show (Scene world cam) = "Scene (" ++ show world ++ ") (" ++ show cam ++ ")"

render :: (RandomGen g, Spectrum s, RayTracer r s) => g -> r -> Scene s -> Image
render gen tracer (Scene world camera) = toImage $ rayTrace gen camera tracer world
