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
insertBoundingBoxes world = world {objects = map boundSceneObject $ objects world}


data BVMaterial = BVMaterial
    deriving (Show)
instance Material BVMaterial RGB where
    brdf _ (Vector u v w) _ _
        | test u v w || test v w u || test w u v = RGB 0.05 0.05 0.05
        | otherwise = RGB 0 0.5 1
        where
            thickness = 0.05
            test a b c = (a <= -0.5 + thickness || a >= 0.5 - thickness) &&
                         (b <= -0.5 + thickness || b >= 0.5 - thickness ||
                          c <= -0.5 + thickness || c >= 0.5 - thickness ||
                          abs (b - c) <= sqrt 2 * thickness)
extractBVHLayer :: Int -> World s -> World RGB
extractBVHLayer depth world = insertBoundingBoxes $ createWorld boxes []
    where
        bvhs = map boundingVolume $ objects world
        extractBoxes :: Int -> BoundingVolume -> [SceneObject RGB]
        extractBoxes 0 bvh = [SceneObject (boundingBox bvh) BVMaterial]
        extractBoxes d bvh = case bvh of
            BoundingVolume _ innerVolumes -> concatMap (extractBoxes $ d-1) innerVolumes
            Bounded _ _ -> []
            TransformedBoundingVolume _ t innerVolume -> map (trans t) (extractBoxes (d-1) innerVolume)
        boxes = concatMap (extractBoxes depth) bvhs

        trans :: Transformation Double -> SceneObject s -> SceneObject s
        trans t (SceneObject s m) = SceneObject (Transformed t s) m
        trans t (SceneLight l) = SceneLight (Transformed t l)
        trans t (Compound objs) = Compound $ map (trans t) objs
