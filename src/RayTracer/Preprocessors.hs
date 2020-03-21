module RayTracer.Preprocessors 
    ( processScene
    , insertBoundingBoxes
    , extractBVHLayer
    ) where

import RayTracer.Core
import RayTracer.Geometry
import RayTracer.Lightning


processScene :: (World s -> World a) -> Scene s -> Scene a
processScene processWorld scene = scene {getWorld = processWorld $ getWorld scene}


insertBoundingBoxes :: World s -> World s
insertBoundingBoxes world = world {objects = [SceneObject $ boundingVolume world]}


bvMaterial :: Material RGB
bvMaterial (Vector u v w)
    | test u v w || test v w u || test w u v = diffuseBRDF $ RGB 0.05 0.05 0.05
    | otherwise = diffuseBRDF $ RGB 0 0.5 1
    where
        thickness = 0.05
        test a b c = (a <= -0.5 + thickness || a >= 0.5 - thickness) &&
                        (b <= -0.5 + thickness || b >= 0.5 - thickness ||
                        c <= -0.5 + thickness || c >= 0.5 - thickness ||
                        abs (b - c) <= sqrt 2 * thickness)
extractBVHLayer :: Int -> World s -> World RGB
extractBVHLayer depth world = insertBoundingBoxes $ createWorld boxes []
    where
        bvhs = boundingVolume world
        extractBoxes :: Int -> BoundingVolume (SceneObject s) -> [SceneObject RGB]
        extractBoxes 0 bvh = [SceneObject $ Surface (boundingBox bvh) bvMaterial]
        extractBoxes d bvh = case bvh of
            BoundingVolume _ innerVolumes -> concatMap (extractBoxes $ d-1) innerVolumes
            Bounded _ _ -> []
            TransformedBoundingVolume _ t innerVolume -> [SceneObject $ Transformed t (extractBoxes (d-1) innerVolume)]
        boxes = extractBoxes depth bvhs
