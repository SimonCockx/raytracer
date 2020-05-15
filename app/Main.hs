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
camera = createPerspectiveCamera 500 300 (Point 2 3 4) (Vector (-2.5) (-3) (-4)) (Vector 0 1 0) (pi/2) (Stratified 48)

createScene :: IO (Scene RGB)
createScene = do
    wood <- readTextureMap "objects/wood.jpg" :: IO (TextureMap RGB)
    let floor = Plane (Point 0 0 0) (Vector 0 1 0)
        left  = translate (-15:: Double) 0 0 `transform` createBox 2 50 50
        right = translate (15::Double) 0 0 `transform` createBox 2 50 50
        back  = translate 0 0 (-15::Double) `transform` createBox 50 50 2
        top   = translate 0 (15::Double) 0 `transform` createBox 50 2 50
        front = translate 0 0 (15::Double) `transform` createBox 50 50 2
        cilinder = translate 0 0.5 (0.5::Double) `transform` (DoubleSided $ createOpenCylinder (Vector 0 1 0) 3 2)
        light = translate 8 6 (0::Double) `transform` createAreaLight (Vector (-3) (-2) 0) 1 1 (RGB 260 260 260)

        world = createWorld [ withMaterial floor (DiffuseTexture wood (\(Vector x y z) -> (x/9, z/9)))
                      , withMaterial cilinder (DiffuseReflective $ RGB 0.8 0.55 0.35)
                      , SceneLight light
                      , withMaterial left (Diffuse $ RGB 0.1 0.05 0)
                      , withMaterial right (Diffuse $ RGB 0.1 0.05 0)
                      , withMaterial back (Diffuse $ RGB 0.1 0.05 0)
                      , withMaterial top (Diffuse $ RGB 0.05 0.05 0.05)
                      , withMaterial front (Diffuse $ RGB 0.1 0.05 0)
                      ]
                      [ Light light
                      , Light $ AmbientLight $ RGB 0.03 0.03 0.03
                      ]
    return $ Scene (insertBoundingBoxes world) camera


rayTracer = MaxDepthPathTracer 16

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
  Scene !world _ <- reflectiveCornell
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


measureRouletteRMSE :: IO ()
measureRouletteRMSE = do
  let res = 150
      refCount = 128
      count = 64
  h <- openFile "out/measurements_roulette/measurements.csv" WriteMode
  hPutStrLn h $ intercalate "," ["alpha", "time", "rmse", "drmse"]
  hClose h
  gens <- getWorkerGens
  Scene !world _ <- reflectiveCornell
  let refCam =
        createPerspectiveCamera
          res
          res
          (Point 0 7 2)
          (Vector 0 0 (-1))
          (Vector 0 1 0)
          (pi / 2)
          (Random (refCount * refCount))
      refId = "ref"
  refImage <- computeSpectralImageAs B gens $ rayTrace refCam (RussianRoulettePathTracer 0.6) world
  saveSpectralImage ("out/measurements_roulette/measurement-" ++ refId ++ ".spectral") refImage
  writeImage ("out/measurements_roulette/" ++ refId ++ ".jpg") (toImage $ gammaCorrectImage refImage)
  forM_ [0.40, 0.42..0.80] $ \alpha -> do
    let spp = count * count
        cam =
          createPerspectiveCamera res res (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi / 2) (Random spp)
        id = "alpha" ++ show (round $ 100*alpha)
    start <- getCPUTime
    !specImageWithError <- computeSpectralImageAs B gens $ rayTraceWithError cam (RussianRoulettePathTracer alpha) world refImage
    evaluate (rnf specImageWithError)
    end <- getCPUTime
    let specImage = computeAs B $ A.map fst specImageWithError
        errImage = computeAs B $ A.map snd specImageWithError
        errSpecImage = A.map (toRGB . Gray) errImage
    saveSpectralImage ("out/measurements_roulette/measurement-" ++ id ++ ".spectral") specImage
    saveSpectralImage ("out/measurements_roulette/measurement-" ++ id ++ "err.spectral") errImage
    writeImage ("out/measurements_roulette/" ++ id ++ ".jpg") (toImage $ gammaCorrectImage specImage)
    writeImage ("out/measurements_roulette/" ++ id ++ "err.jpg") (toImage $ gammaCorrectImage errSpecImage)
    let time = fromIntegral (end - start) * 1.0e-12 :: Double
        se = A.zipWith (\c1 c2 -> let RGB r g b = c1 ^-^ c2 in r ** 2 + g ** 2 + b ** 2) refImage specImage
        sqErrSe = A.zipWith (\a b -> 4 * a * b * b) se errImage
        count = let Sz (r :. c) = size se in fromIntegral $ r*c
        mse = (/count) $ A.sum se
        errMse = (/count) $ sqrt $ A.sum sqErrSe
        rmse = sqrt mse
        errRmse = errMse / (2*rmse)
        measurement = intercalate "," [show alpha, show time, show rmse, show errRmse]
    h <- openFile "out/measurements_roulette/measurements.csv" AppendMode
    hPutStrLn h measurement
    hClose h


measureBranching :: IO ()
measureBranching = do
  let res = 150
      refCount = 32
      count = 64
  h <- openFile "out/measurements_branching/measurements.csv" WriteMode
  hPutStrLn h $ intercalate "," ["bf", "spp", "time", "rmse", "drmse"]
  hClose h
  gens <- getWorkerGens
  Scene !world _ <- diffuseCornell
  let refCam =
        createPerspectiveCamera
          res
          res
          (Point 0 7 2)
          (Vector 0 0 (-1))
          (Vector 0 1 0)
          (pi / 2)
          (Random (refCount * refCount))
      refId = "ref"
  refImage <- computeSpectralImageAs B gens $ rayTrace refCam (MaxDepthPathTracer 3) world
  saveSpectralImage ("out/measurements_branching/measurement-" ++ refId ++ ".spectral") refImage
  writeImage ("out/measurements_branching/" ++ refId ++ ".jpg") (toImage $ gammaCorrectImage refImage)
  forM_ [(1, []), (2, []), (3, [])] $ \(bf, sppValues) ->
    forM_ sppValues $ \spp -> do
      let cam =
            createPerspectiveCamera res res (Point 0 7 2) (Vector 0 0 (-1)) (Vector 0 1 0) (pi / 2) (Random spp)
          id = "bf" ++ show bf ++ "-spp" ++ show spp
      start <- getCPUTime
      !specImageWithError <- computeSpectralImageAs B gens $ rayTraceWithError cam (BranchingDirectionalMaxDepthPathTracer 3 bf) world refImage
      evaluate (rnf specImageWithError)
      end <- getCPUTime
      let specImage = computeAs B $ A.map fst specImageWithError
          errImage = computeAs B $ A.map snd specImageWithError
          errSpecImage = A.map (toRGB . Gray) errImage
      saveSpectralImage ("out/measurements_branching/measurement-" ++ id ++ ".spectral") specImage
      saveSpectralImage ("out/measurements_branching/measurement-" ++ id ++ "err.spectral") errImage
      writeImage ("out/measurements_branching/" ++ id ++ ".jpg") (toImage $ gammaCorrectImage specImage)
      writeImage ("out/measurements_branching/" ++ id ++ "err.jpg") (toImage $ gammaCorrectImage errSpecImage)
      let time = fromIntegral (end - start) * 1.0e-12 :: Double
          se = A.zipWith (\c1 c2 -> let RGB r g b = c1 ^-^ c2 in r ** 2 + g ** 2 + b ** 2) refImage specImage
          sqErrSe = A.zipWith (\a b -> 4 * a * b * b) se errImage
          count = let Sz (r :. c) = size se in fromIntegral $ r*c
          mse = (/count) $ A.sum se
          errMse = (/count) $ sqrt $ A.sum sqErrSe
          rmse = sqrt mse
          errRmse = errMse / (2*rmse)
          measurement = intercalate "," [show bf, show spp, show time, show rmse, show errRmse]
      h <- openFile "out/measurements_branching/measurements.csv" AppendMode
      hPutStrLn h measurement
      hClose h


main :: IO ()
main = do
  gens <- getWorkerGens
  Scene world _ <- createScene
  specImage <- computeSpectralImageAs B gens $ rayTrace camera rayTracer world
  timeStamp <- formatTime defaultTimeLocale "%Y.%m.%d %H.%M.%S" <$> getZonedTime
  saveSpectralImage ("out/measurements_caustic/ref" ++ timeStamp ++ ".spectral") specImage
  writeImage ("out/measurements_caustic/ref" ++ timeStamp ++ ".jpg") (toImage $ gammaCorrectImage specImage)
  