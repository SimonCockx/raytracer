module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image)

import Data.Time
import Scenes


camera :: PerspectiveCamera
camera = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 4)

createScene :: IO (Scene RGB)
createScene = do
    teaPotObj <- readObjFile "objects/teaPot.obj"
    let floor = translate (-2) (-3) (-6::Double) `transform` createAABox 30 2 30
        teaPot = translate (-2::Double) (-2) (-6) `transform` scaleUni (2 :: Double) `transform` rotateY (pi/15 :: Double) `transform` teaPotObj

        world = createWorld [ SceneObject teaPot (Diffuse $ RGB 0.8 0.5 0.9)
                            , simpleObject floor
                            ]
                            [ Light $ Transformed (translate 2 1 (-6 :: Double) `transform` rotateZ (-pi/3)) $ AreaLight 3 3 $ RGB 1 1 1
                            ]
    return $ Scene (insertBoundingBoxes world) camera

rayTracer = SpectrumIndependentRayTracer $ Random 10

renderFast :: (Spectrum s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene) 
                          (createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 2))
    return $ render gen (SpectrumIndependentRayTracer $ Random 1) fastScene

justRender :: (Spectrum s) => IO (Scene s) -> IO Image
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
    --(show <$> scene) >>= writeFile "test.txt"
    image <- justRender scene
    saveAs "explicit_area_light_test" image
    display image

    --images <- mapM (\n -> render gen (BVHLayerTracer n) <$> scene) [0..10]
    -- mapM (\(n, img) -> saveAs ("BHV_test" ++ show n) img) $ zip [0..] images
    -- return ()
