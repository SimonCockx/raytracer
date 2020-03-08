module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image)

import Data.Time
import Scenes

camera :: PerspectiveCamera
camera = createPerspectiveCamera (600 `div` 2) (400 `div` 2) (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (Random 1)

createScene :: IO (Scene Gray)
createScene = do
    teaPot <- readObjFile "objects/teaPot.obj"
    teaPotList <- readObjFileAsList "objects/teaPot.obj"
    let teaPot1  = Transformed ((translate (-4::Double) (-2) (-9)) `transform` (scaleUni 3.5)) teaPot
        teaPot2  = Transformed ((translate (4::Double) (-2) (-9)) `transform` (scaleUni 3.5)) teaPotList

        world = World [ simpleObject teaPot1
                      , simpleObject teaPot2
                      ]
                      [ Light $ LongRangePointLight (Point (-4) 1 (-5)) $ 3 *^ (white :: Gray)
                      , Light $ LongRangePointLight (Point 4 1 (-5)) $ 3 *^ (white :: Gray)
                      ]
        acceleratedWorld = insertBoundingBoxes world
    return $ Scene acceleratedWorld camera

rayTracer = SpectrumIndependentRayTracer


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
    scene <- coloryScene
    let ezScene = Scene (getWorld scene) camera
        image = render gen rayTracer ezScene
    display image
