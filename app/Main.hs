module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image)

import Data.Time
import Scenes


camera :: PerspectiveCamera
camera = createPerspectiveCamera 1920 1050 (Point 5 1 (-1.4)) (Vector (-1) (-0.3) (-0.6)) (Vector 0 1 0) (pi/2) (RegularGrid 2)

createScene :: IO (Scene RGB)
createScene = do
    teaPotObj   <- readObjFile "objects/magikarp.obj"
    let floor = translate (-2) (-3) (-9::Double) `transform` createAABox 20 2 20
        teaPot = (translate (-0::Double) (-0) (-5)) `transform` (rotateX (-pi/2::Double)) `transform` (scaleUni (0.6::Double)) `transform` teaPotObj

        world = createWorld [ SceneObject teaPot (Diffuse $ RGB 0.8 0.5 0.9)
                            --, simpleObject floor
                            ]
                            [ --Light $ AmbientLight $ RGB 1 1 1
                            ]
    return $ Scene (insertBoundingBoxes world) camera1

rayTracer = DiffuseRayTracer

renderFast :: (Spectrum s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene)
                          (createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 2))
    return $ render gen (SpectrumIndependentRayTracer $ Random 1) fastScene

justRender :: (Spectrum s) => IO (Scene s) -> IO Image
justRender getScene = do
    scene <- getScene
    let sceneWithMyCamera = Scene (getWorld scene) camera
    return $ render gen rayTracer sceneWithMyCamera

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
    -- scene >>= print
    -- (show <$> scene) >>= writeFile "test.txt"
    -- image <- justRender scene
    -- saveAs "bhv_test" image
    -- display image

    mapM_ (\n -> do
        timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
        putStrLn $ timeStamp ++ " - Iteration " ++ show n
        img <- justRender $ processScene (extractBVHLayer n) <$> scene
        saveAs ("BHV_test" ++ show n) img) $ [0..30]
    image <- justRender scene
    saveAs "bhv_test" image
    display image

