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

        world = createWorld [ withMaterial sphere (Diffuse $ RGB 1 1 0)
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
computeMeanRadiance img = (\(RGB r g b) -> (r + g + b)/count) $ A.foldlS (^+^) black img
  where
    Sz (r :. c) = size img
    count = fromIntegral $ r * c

measureConvergence :: IO ()
measureConvergence = do
  let res = 150
--  h <- openFile "out/measurements2/measurements.csv" WriteMode
--  hPutStrLn h $ intercalate "," ["tracer", "area", "spp", "time", "rmse", "drmse"]
--  hClose h
  gens <- getWorkerGens
  forM_ ([1,2..21] ++ [23,25]) $ \area -> do
    Scene !world _ <- createCornellWithArea area
    forM_
      [ (AnyRayTracer $ MaxDepthPathTracer 1, "direct", AnyRayTracer $ MaxDepthPathTracer 1, 32)
      , (AnyRayTracer $ DirectionalMaxDepthPathTracer 3, "directional", AnyRayTracer $ MaxDepthPathTracer 3, 32)
      , (AnyRayTracer $ MaxDepthPathTracer 3, "hybride", AnyRayTracer $ MaxDepthPathTracer 3, 32)
      ] $ \(tracer, tracerName, refTracer, refCount) -> do
      let refCam =
            createPerspectiveCamera
              res
              res
              (Point 0 7 2)
              (Vector 0 0 (-1))
              (Vector 0 1 0)
              (pi / 2)
              (Random (refCount * refCount))
          refId = tracerName ++ "-A" ++ show area ++ "-ref"
      refImage <- computeSpectralImageAs B gens $ rayTrace refCam refTracer world
      saveSpectralImage ("out/measurements2/measurement-" ++ refId ++ ".spectral") refImage
      writeImage ("out/measurements2/" ++ refId ++ ".jpg") (toImage $ gammaCorrectImage refImage)
      forM_ [2,4 .. 20] $ \n -> do
        let spp = n * n
            cam =
              createPerspectiveCamera res res (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi / 2) (Random (n * n))
            id = tracerName ++ "-A" ++ show area ++ "-spp" ++ show spp
        start <- getCPUTime
        -- TODO: hier zit nog ergens een geheugenlek in...
        !specImageWithError <- computeSpectralImageAs B gens $ rayTraceWithError cam tracer world refImage
        evaluate (rnf specImageWithError)
        end <- getCPUTime
        let specImage = computeAs B $ A.map fst specImageWithError
            errImage = computeAs B $ A.map snd specImageWithError
            errSpecImage = A.map (toRGB . Gray) errImage
        saveSpectralImage ("out/measurements2/measurement-" ++ id ++ ".spectral") specImage
        saveSpectralImage ("out/measurements2/measurement-" ++ id ++ "err.spectral") errImage
        writeImage ("out/measurements2/" ++ id ++ ".jpg") (toImage $ gammaCorrectImage specImage)
        writeImage ("out/measurements2/" ++ id ++ "err.jpg") (toImage $ gammaCorrectImage errSpecImage)
        let time = fromIntegral (end - start) * 1.0e-12 :: Double
            se = A.zipWith (\c1 c2 -> let RGB r g b = c1 ^-^ c2 in r ** 2 + g ** 2 + b ** 2) refImage specImage
            sqErrSe = A.zipWith (\a b -> 4 * a * b * b) se errImage
            count = let Sz (r :. c) = size se in fromIntegral $ r*c
            mse = (/count) $ A.sum se
            errMse = (/count) $ sqrt $ A.sum sqErrSe
            rmse = sqrt mse
            errRmse = errMse / (2*rmse)
            measurement = force $! intercalate "," [tracerName, show area, show spp, show time, show rmse, show errRmse]
        h <- openFile "out/measurements2/measurements.csv" AppendMode
        hPutStrLn h measurement
        hClose h

main :: IO ()
main = 
  measureConvergence
  