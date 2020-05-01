{-# LANGUAGE ExistentialQuantification #-}

module RayTracer.Core.Scene
    ( Scene (..)
    , render
    , gammaCorrectedRender
    ) where

import RayTracer.Core.World
import RayTracer.Core.Camera
import RayTracer.Core.RayTracer
import RayTracer.Lightning
import RayTracer.Random
import Data.Massiv.Array (WorkerStates)

-- | A type representing a scene of shapes.
data Scene s = forall cam. (Camera cam, Show cam) => Scene {getWorld :: World s, getCamera :: cam}

instance Show (Scene s) where
    show (Scene world cam) = "Scene (" ++ show world ++ ") (" ++ show cam ++ ")"

render :: (RayTracer r s, Show s) => WorkerStates Gen -> r -> Scene s -> Image
render gens tracer (Scene world camera) = toImage gens $ rayTrace camera tracer world

gammaCorrectedRender :: (RayTracer r s, Show s) => WorkerStates Gen -> r -> Scene s -> Image
gammaCorrectedRender gens tracer (Scene world camera) = toImage gens $ gammaCorrectImageM $ rayTrace camera tracer world
