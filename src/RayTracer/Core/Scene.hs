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
data Scene s = forall cam. (Camera cam, Show cam) => Scene {getWorld :: (World s), getCamera :: cam}

instance (Show s) => Show (Scene s) where
    show (Scene world cam) = "Scene (" ++ show world ++ ") (" ++ show cam ++ ")"

render :: (RayTracer r s) => Gen -> r -> Scene s -> Image
render gen tracer (Scene world camera) = fst $ (`runRand` gen) $ do
    spectralImage <- rayTrace camera tracer world
    return $ toImage spectralImage
