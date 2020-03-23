{-# LANGUAGE OverloadedStrings #-}

module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image, encode)
import Data.Massiv.Array hiding (map, mapM, mapM_, zip)

import Data.Time
import System.IO
import System.Random.SplitMix
import Scenes


camera :: PerspectiveCamera
camera = createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)

createScene :: IO (Scene RGB)
createScene = do
    let sphere = translate 0 0 (-3::Double) `transform` rotateZ (pi/4::Double) `transform` createAABox 2 2 0.01

        world = createWorld [ SceneObject sphere $ Diffuse $ RGB 1 0 0.5
                            ]
                            [ Light $ LongRangePointLight (Point 2 0 0) $ RGB 1 1 1
                            ]
    return $ Scene (insertBoundingBoxes world) camera

rayTracer = SpectrumIndependentRayTracer $ Random 100

renderFast :: (Spectrum s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene)
                          (createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1))
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


measureRadiance :: IO ()
measureRadiance = do
    let lightPoint = 175 :. 150
        penumbraPoint = 170 :. 60
        umbraPoint = 150 :. 60
        shadowRays = [1..1000]
        measurements = 100
    Scene world camera <- createScene
    h <- openFile "out/lightRads.csv" WriteMode
    hClose h
    h <- openFile "out/penumbraRads.csv" WriteMode
    hClose h
    h <- openFile "out/umbraRads.csv" WriteMode
    hClose h
    (`mapM_` shadowRays) $ \n -> do
        g <- newSMGen
        let randImage = rayTrace camera (SpectrumIndependentRayTracer $ Random n) world
            (spectralImages, newGen) = (`runRand` g) $ replicateM measurements randImage
            lightRads = map (\spectralImage -> let RGB r _ _ = evaluate' spectralImage lightPoint in r) spectralImages
            penumbraRads = map (\spectralImage -> let RGB r _ _ = evaluate' spectralImage penumbraPoint in r) spectralImages
            umbraRads = map (\spectralImage -> let RGB r _ _ = evaluate' spectralImage umbraPoint in r) spectralImages
        h <- openFile "out/lightRads.csv" AppendMode
        hPutStr h $ show n ++ ","
        (`mapM_` (zip [0..] lightRads)) $ \(i, r) -> do
            when (i /= 0) $ hPutChar h ','
            hPutStr h $ show r
        hPutStrLn h ""
        hClose h
        h <- openFile "out/penumbraRads.csv" AppendMode
        hPutStr h $ show n ++ ","
        (`mapM_` (zip [0..] penumbraRads)) $ \(i, r) -> do
            when (i /= 0) $ hPutChar h ','
            hPutStr h $ show r
        hPutStrLn h ""
        hClose h
        h <- openFile "out/umbraRads.csv" AppendMode
        hPutStr h $ show n ++ ","
        (`mapM_` (zip [0..] umbraRads)) $ \(i, r) -> do
            when (i /= 0) $ hPutChar h ','
            hPutStr h $ show r
        hPutStrLn h ""
        hClose h
        print n
    return ()


main :: IO ()
main = do
    -- measureRadiance
    let scene = createScene
    -- -- scene >>= print
    -- (show <$> scene) >>= writeFile "test.txt"
    image <- justRender scene
    saveAs "aa" image
    display image

    -- mapM_ (\n -> do
    --     timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
    --     putStrLn $ timeStamp ++ " - Iteration " ++ show n
    --     img <- justRender $ processScene (extractBVHLayer n) <$> scene
    --     saveAs ("BHV_test" ++ show n) img) $ [0..30]
    -- image <- justRender scene
    -- saveAs "bhv_test" image
    -- display image

