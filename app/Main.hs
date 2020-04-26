{-# LANGUAGE OverloadedStrings #-}

module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image, encode)
import Data.Massiv.Array hiding (map, mapM, mapM_, zip)
import qualified Data.Massiv.Array as A

import Data.Time
import System.IO
import System.Random.SplitMix
import Scenes


camera :: PerspectiveCamera
camera = createPerspectiveCamera 500 360 (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/1.5) (RegularGrid 16)

createScene :: IO (Scene RGB)
createScene = do
    teaPotObj <- readObjFile "objects/teaPot.obj"
    let floor = translate 0 (0::Double) 0 `transform` createBox 50 2 50
        left  = translate (-8:: Double) 0 0 `transform` createBox 2 50 50
        right = translate (8::Double) 0 0 `transform` createBox 2 50 50
        back  = translate 0 0 (-16::Double) `transform` createBox 50 50 2
        top = translate 0 (16::Double) 0 `transform` createBox 50 2 50
        sphere = translate (4:: Double) 3 (-9.5) `transform` createSphere 2
        block = translate (-2::Double) 2 (-8.5) `transform` rotateY (pi/6::Double) `transform` createBox 4 6 4
        light = translate 0 14.99 (-10::Double) `transform` createAreaLight (Vector 0 1 0) 4 4 (RGB 9 9 9)

        world = createWorld [ withMaterial sphere Reflective
                      , simpleObject floor
                      , withMaterial left (Diffuse $ RGB 1 0 0)
                      , withMaterial back (Diffuse $ RGB 1 1 1)
                      , withMaterial right (Diffuse $ RGB 0 1 0)
                      , simpleObject block
                      , simpleObject top
                      , SceneLight light
                      ]
                      [ Light light
                      ]
    return $ Scene (insertBoundingBoxes world) camera


data AnyRayTracer s = forall a. (RayTracer a s) => AnyRayTracer a
instance (Spectrum s1, s1 ~ s2) => RayTracer (AnyRayTracer s1) s2 where
    traceRay (AnyRayTracer t) = traceRay t

rayTracer = RussianRoulettePathTracer 0.7

tracers :: [AnyRayTracer RGB]
tracers = [ AnyRayTracer $ DirectLightningTracer $ Random 1
          , AnyRayTracer $ SpecificDepthPathTracer 0
          , AnyRayTracer $ SpecificDepthPathTracer 1
          , AnyRayTracer $ SpecificDepthPathTracer 2
          , AnyRayTracer $ SpecificDepthPathTracer 3
          , AnyRayTracer $ RussianRoulettePathTracer 0.5
          ]

renderFast :: (Spectrum s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene)
                          (createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1))
    return $ render gen (DirectLightningTracer $ Random 1) fastScene

justRender :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
justRender getScene = do
    scene <- getScene
    let sceneWithMyCamera = Scene (getWorld scene) camera
    return $ render gen rayTracer sceneWithMyCamera

justRenderWith :: (Spectrum s, Show s, RayTracer a s) => a -> IO (Scene s) -> IO Image
justRenderWith tracer getScene = do
    scene <- getScene
    let sceneWithMyCamera = Scene (getWorld scene) camera
    return $ render gen tracer sceneWithMyCamera

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
        let randImage = rayTrace camera (DirectLightningTracer $ Random n) world
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


traceWithMaxDepth :: (Spectrum s, Show s) => Int -> IO (Scene s) -> IO Image
traceWithMaxDepth depth getScene = do
    scene <- getScene
    let Scene world _ = scene -- TODO: get camera from scene instead (problem with existential types...)
        contributions = fst $ (`runRand` gen) $ (`mapM` [0..depth]) $ \d ->
            inverseGammaCorrectImage <$> rayTrace camera (SpecificDepthPathTracer d) world
        image = toImage $ gammaCorrectImage $ foldr1 (A.zipWith (^+^)) contributions
    return image


main :: IO ()
main = do
    let getScene = createScene
    -- getScene >>= print
    -- (show <$> getScene) >>= writeFile "test.txt"
    -- image <- justRenderWith (SpecificDepthPathTracer 0) getScene
    -- saveAs "depth0" image
    -- image <- justRenderWith (SpecificDepthPathTracer 1) getScene
    -- saveAs "depth1" image
    -- image <- justRenderWith (SpecificDepthPathTracer 2) getScene
    -- saveAs "depth2" image
    -- image <- justRenderWith (SpecificDepthPathTracer 3) getScene
    -- saveAs "depth3" image
    image <- justRenderWith (MaxDepthPathTracer 3) getScene
    saveAs "depthMax" image
