module RayTracer.Preprocessors 
    ( processScene
    , insertBoundingBoxes
    , extractBVHLayer
    ) where

import RayTracer.Core
import RayTracer.Geometry
import RayTracer.Lightning

import Data.Maybe (mapMaybe, maybeToList)


processScene :: (World s -> World a) -> Scene s -> Scene a
processScene processWorld scene = scene {getWorld = processWorld $ getWorld scene}


insertBoundingBoxes :: World s -> World s
insertBoundingBoxes world = world {objects = [SceneObject $ boundedObjectNode world]}


bvMaterial :: ProceduralDiffuseTexture RGB
bvMaterial = ProceduralDiffuseTexture tex
    where 
        tex (Vector u v w)
            | test u v w || test v w u || test w u v = RGB 0.05 0.05 0.05
            | otherwise = RGB 0 0.5 1
            where
                thickness = 0.05
                test a b c = (a <= -0.5 + thickness || a >= 0.5 - thickness) &&
                                (b <= -0.5 + thickness || b >= 0.5 - thickness ||
                                c <= -0.5 + thickness || c >= 0.5 - thickness ||
                                abs (b - c) <= sqrt 2 * thickness)

extractBVHLayer :: Int -> World s -> World RGB
extractBVHLayer depth world = insertBoundingBoxes $ World (map SceneObject $ maybeToList boxes) []
    where
        root = boundedObjectNode world
        extractObjectBoxes :: Int -> BoundedObjectNode s -> Maybe (BoundedObjectNode RGB)
        extractObjectBoxes 0 bvh = Just $ MaterialNode bvMaterial $ BoxNode $ boundingBox bvh
        extractObjectBoxes d bvh = case bvh of
            ObjectBranchNode box innerNodes -> case mapMaybe (extractObjectBoxes $ d-1) innerNodes of
                []     -> Nothing
                [node] -> Just node
                l      -> Just $ ObjectBranchNode box l
            MaterialNode _ shapeNode -> extractShapeBoxes d shapeNode
            TransformedObjectNode box tr objectNode -> TransformedObjectNode box tr <$> extractObjectBoxes (d-1) objectNode
        extractShapeBoxes :: Int -> BoundedShapeNode -> Maybe (BoundedObjectNode RGB)
        extractShapeBoxes 0 bvh = Just $ MaterialNode bvMaterial $ BoxNode $ boundingBox bvh
        extractShapeBoxes d bvh = case bvh of
            ShapeBranchNode box innerNodes -> case mapMaybe (extractShapeBoxes $ d-1) innerNodes of
                []     -> Nothing
                [node] -> Just node
                l      -> Just $ ObjectBranchNode box l
            ShapeNode box shape -> if d == 1 then Just $ MaterialNode whiteMaterial $ ShapeNode box shape else Nothing
            BoxNode _ -> Nothing
            TransformedShapeNode box tr shapeNode -> TransformedObjectNode box tr <$> extractShapeBoxes (d-1) shapeNode
        boxes = extractObjectBoxes depth root
