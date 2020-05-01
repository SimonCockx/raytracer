module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image, encode)
import Data.Massiv.Array hiding (map, mapM, mapM_, zip)
import qualified Data.Massiv.Array as A

import Data.Time
import System.IO
import Scenes
import Control.Scheduler (getWorkerId)
import Control.Monad (liftM2)

seeds :: [Seed]
seeds = [11, 23, 55, 83, 145, 250, 954, 1010]

getWorkerGens :: IO (WorkerStates Gen)
getWorkerGens = initWorkerStates Par $ createGen . (seeds !!) . getWorkerId

camera :: PerspectiveCamera
camera = createPerspectiveCamera 300 300 (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 32)

createScene :: IO (Scene RGB)
createScene = do
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


rayTracer = DirectionalMaxDepthPathTracer 3

renderFast :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
renderFast getScene = do
    scene <- getScene
    gens <- getWorkerGens
    let fastScene = Scene (insertBoundingBoxes $ getWorld scene)
                          (createPerspectiveCamera 300 200 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1))
    return $ gammaCorrectedRender gens (DirectLightningTracer $ Random 1) fastScene

justRender :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
justRender getScene = do
    scene <- getScene
    gens <- getWorkerGens
    -- let sceneWithMyCamera = Scene (getWorld scene) camera
    return $ render gens rayTracer scene

justRenderWith :: (Spectrum s, Show s, RayTracer a s) => a -> IO (Scene s) -> IO Image
justRenderWith tracer getScene = do
    scene <- getScene
    gens <- getWorkerGens
    -- let sceneWithMyCamera = Scene (getWorld scene) camera
    return $ render gens tracer scene


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


displayBHVLayers :: Int -> String -> IO (Scene s) -> IO ()
displayBHVLayers depth name getScene = do
  scene <- getScene
  gens <- getWorkerGens
  mapM_
    (\n -> do
       timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
       putStrLn $ timeStamp ++ " - Iteration " ++ show n
       print $ processScene (extractBVHLayer n) scene
       let img = render gens DiffuseRayTracer $ processScene (extractBVHLayer n) scene
       saveAs (name ++ show n) img)
    [0 .. depth]


traceWithMaxDepth :: (Spectrum s, Show s) => Int -> IO (Scene s) -> IO Image
traceWithMaxDepth depth getScene = do
    scene <- getScene
    gens <- getWorkerGens
    let Scene world _ = scene -- TODO: get camera from scene instead (problem with existential types...)
        contributions = (`map` [0..depth]) $ \d ->
            inverseGammaCorrectImageM $ rayTrace camera (SpecificDepthPathTracer d) world
        image = toImage gens $ gammaCorrectImageM $ foldr1 (A.zipWith (liftM2 (^+^))) contributions
    return image


main :: IO ()
main = do
    let getScene = createScene
    -- getScene >>= print
    -- (show <$> getScene) >>= writeFile "test.txt"
    Scene world _ <- getScene
    gens <- getWorkerGens
    let image = toImage gens $ gammaCorrectImageM $ rayTrace camera rayTracer world
    saveAs "roulette0.7" image
