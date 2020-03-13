module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image)

import Data.Time
import Scenes


camera :: PerspectiveCamera
camera = createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)

createScene :: IO (Scene RGB)
createScene = do
    obj      <- readObjFile "objects/teapot.obj"
    let teapot = translate (0::Double) (-1) (-4) `transform` scaleUni (2::Double) `transform` obj
    let floor = translate (-2) (-3) (-9::Double) `transform` createAABox 20 2 20

        world = World [ SceneObject teapot (Diffuse $ RGB 0.8 0.5 0.9)
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 3 1 1)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 1 3 1)
                      ]
    return $ Scene (insertBoundingBoxes world) camera

rayTracer = IntersectionTestsTracer 100

renderFast :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene) 
                          (createPerspectiveCamera 200 150 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 2))
    return $ render gen (SpectrumIndependentRayTracer $ Random 1) fastScene

justRender :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
justRender getScene = render gen rayTracer <$> getScene

gen :: Gen
gen = createGen 29


save :: Image -> IO ()
save image = do
    timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
    writeImage ("results/" ++ timeStamp ++ ".jpg") image

saveAs :: String -> Image -> IO ()
saveAs name image = do
    timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
    writeImage ("out/results/" ++ timeStamp ++ " - " ++ name ++ ".jpg") image

display :: Image -> IO ()
display = displayImageUsing defaultViewer True


main :: IO ()
main = do
    let scene = createScene
    --scene >>= print
    image <- justRender scene
    saveAs "BVH_test" image
    display image

    --images <- mapM (\n -> render gen (BVHLayerTracer n) <$> scene) [0..10]
    -- mapM (\(n, img) -> saveAs ("BHV_test" ++ show n) img) $ zip [0..] images
    -- return ()
