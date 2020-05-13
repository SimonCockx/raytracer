module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image, encode)
import Data.Massiv.Array hiding (map, mapM, mapM_, zip, forM_, foldlM_)
import qualified Data.Massiv.Array as A

import Data.Time
import System.IO
import Scenes
import TracerIO
import Control.Scheduler (getWorkerId)
import Control.Monad (liftM2, forM_, foldM_, when)
import Data.VectorSpace (magnitudeSq)
import System.CPUTime (getCPUTime)
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.List (intercalate)

seeds :: [Seed]
seeds = [11, 23, 55, 83, 145, 250, 954, 1010]

getWorkerGens :: IO (WorkerStates Gen)
getWorkerGens = initWorkerStates Par $ createGen . (seeds !!) . getWorkerId

camera :: PerspectiveCamera
camera = createPerspectiveCamera 100 100 (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (Random 512)

createCornellWithArea :: Double -> IO (Scene RGB)
createCornellWithArea area = do
    let floor = translate 0 (0::Double) 0 `transform` createBox 50 2 50
        left  = translate (-8:: Double) 0 0 `transform` createBox 2 50 50
        right = translate (8::Double) 0 0 `transform` createBox 2 50 50
        back  = translate 0 0 (-16::Double) `transform` createBox 50 50 2
        top = translate 0 (16::Double) 0 `transform` createBox 50 2 50
        sphere = translate (4:: Double) 3 (-9.5) `transform` createSphere 2
        block = translate (-2::Double) 2 (-8.5) `transform` rotateY (pi/6::Double) `transform` createBox 4 6 4
        side = sqrt area
        light = translate 0 14.999999 (-10::Double) `transform` createAreaLight (Vector 0 1 0) side side ((16/area) *^ RGB 9 9 9)

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
    gammaCorrectedRender gens (DirectLightningTracer $ Random 1) fastScene

justRender :: (Spectrum s, Show s) => IO (Scene s) -> IO Image
justRender getScene = do
    scene <- getScene
    gens <- getWorkerGens
    -- let sceneWithMyCamera = Scene (getWorld scene) camera
    render gens rayTracer scene

justRenderWith :: (Spectrum s, Show s, RayTracer a s) => a -> IO (Scene s) -> IO Image
justRenderWith tracer getScene = do
    scene <- getScene
    gens <- getWorkerGens
    -- let sceneWithMyCamera = Scene (getWorld scene) camera
    render gens tracer scene


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
       img <- gammaCorrectedRender gens DiffuseRayTracer $ processScene (extractBVHLayer n) scene
       saveAs (name ++ show n) img)
    [0 .. depth]

computeMSE :: (Source r Ix2 RGB) => SpectralImageR r RGB -> SpectralImageR r RGB -> Double
computeMSE img1 img2 = (/count) $ A.sum $ A.zipWith (\c1 c2 -> let RGB r g b = c1 ^-^ c2 in r**2 + g**2 + b**2) img1 img2
  where
    Sz (r :. c) = size img1
    count = fromIntegral $ r * c

computeRMSE :: (Source r Ix2 RGB) => SpectralImageR r RGB -> SpectralImageR r RGB -> Double
computeRMSE img1 img2 = sqrt $ computeMSE img1 img2

computeMeanRadiance :: (Source r Ix2 RGB) => SpectralImageR r RGB -> Double
computeMeanRadiance img = (\(RGB r g b) -> sqrt (r**2 + g**2 + b**2)/count) $ A.foldlS (^+^) black img
  where
    Sz (r :. c) = size img
    count = fromIntegral $ r * c

measureDepthConvergence :: IO ()
measureDepthConvergence = do
  let res = 150
      spp = 1024
      cam = createPerspectiveCamera res res (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (Random spp)
  h <- openFile "out/measurements_depth/measurements.csv" WriteMode
  hPutStrLn h $ intercalate "," ["depth", "mean radiance"]
  hClose h
  gens <- getWorkerGens
  Scene !world _ <- createCornellWithArea 16
  forM_ [0..15] $ \depth -> do
    let refId = "d" ++ show depth
    !specImage <- computeSpectralImageAs B gens $ rayTrace cam (SpecificDepthPathTracer depth) world
    evaluate (rnf specImage)
    saveSpectralImage ("out/measurements_depth/" ++ refId ++ ".spectral") specImage
    writeImage ("out/measurements_depth/" ++ refId ++ ".jpg") $ toImage $ gammaCorrectImage specImage
    let mean = computeMeanRadiance specImage
    h <- openFile "out/measurements_depth/measurements.csv" AppendMode
    hPutStrLn h $ intercalate "," [show depth, show mean]
    hClose h


main :: IO ()
main = do
  let res = 150
      spp = 1024
      cam = createPerspectiveCamera res res (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (Random spp)
  gens <- getWorkerGens
  Scene !world _ <- createCornellWithArea 16
  !specImage <- computeSpectralImageAs B gens $ rayTrace cam (MaxDepthPathTracer 16) world
  saveSpectralImage "out/measurements_depth/referentie.spectral" specImage
  writeImage "out/measurements_depth/referentie.jpg" $ toImage $ gammaCorrectImage specImage
  