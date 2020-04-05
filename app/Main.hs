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
camera = createPerspectiveCamera 50 30 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 3)

createScene :: IO (Scene RGB)
createScene = do
    teaPotObj <- readObjFile "objects/teaPot.obj"
    vlekje <- readTextureMap "objects/vlekje.png" :: IO (TextureMap RGB)
    let floor = translate 0 (0::Double) 0 `transform` createBox 50 2 50
        left  = translate (-8:: Double) 0 0 `transform` createBox 2 50 50
        right = translate (8::Double) 0 0 `transform` createBox 2 50 50
        back  = translate 0 0 (-16::Double) `transform` createBox 50 50 2
        top = translate 0 (16::Double) 0 `transform` createBox 50 2 50
        teaPot1 = Transformed ((translate (5::Double) 9 (-13)) `transform` (scaleUni (3::Double)) `transform` rotateY (-pi/6::Double)) teaPotObj
        teaPot2 = Transformed ((translate (0::Double) 1 (-10)) `transform` (scaleUni 4)) teaPotObj
        light = Transformed (translate 0 14.5 (-8::Double)) $ AreaLight 4 4 $ RGB 2 2 2

        world = World [ withMaterial teaPot1 (DiffuseTexture vlekje (\(Vector u v w) -> (u, v)))
                      , withMaterial teaPot2 (Diffuse $ RGB 0.6 0.1 0.3)
                      , simpleObject floor
                      , withMaterial left (Diffuse $ RGB 1 0 0)
                      , withMaterial back (Diffuse $ RGB 0 1 0)
                      , withMaterial right (Diffuse $ RGB 0 0 1)
                      , simpleObject top
                      , SceneLight light
                      ]
                      [ Light light
                      ]
    return $ Scene (insertBoundingBoxes world) camera

rayTracer = SpectrumIndependentRayTracer $ RegularGrid 3

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
    Scene world camera <- softShadowScene
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


displayBHVLayers :: Int -> String -> IO (Scene s) -> IO ()
displayBHVLayers depth name scene = mapM_ (\n -> do
    timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
    putStrLn $ timeStamp ++ " - Iteration " ++ show n
    scene >>= print . processScene (extractBVHLayer n)
    img <- render gen DiffuseRayTracer . processScene (extractBVHLayer n) <$> scene
    saveAs (name ++ show n) img) $ [0..depth]


main :: IO ()
main = do
    let getScene = processScene insertBoundingBoxes <$> softShadowScene
    -- getScene >>= print
    -- (show <$> getScene) >>= writeFile "test.txt"
    image <- justRender getScene
    saveAs "test" image
    display image
