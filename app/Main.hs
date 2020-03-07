module Main where

import RayTracer
import Data.Massiv.Array.IO hiding (Image)

import Data.Time
import Scenes

camera :: PerspectiveCamera
camera = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)

createScene :: IO (Scene RGB)
createScene = do
    let elipse = scale (2 :: Double) 1 1 `transform` createSphere 1
        elipse1 = translate (-4) 0 (-10::Double) `transform` elipse
        elipse2 = translate 0 0 (-10::Double) `transform` rotateY (pi/2::Double) `transform` elipse
        elipse3 = translate 4 0 (-10::Double) `transform` rotateZ (pi/2::Double) `transform` elipse
        sphere  = translate 0 4 (-10::Double) `transform` createSphere 1

        world = World [ simpleObject elipse1
                      , simpleObject elipse2
                      , simpleObject elipse3
                      , simpleObject sphere
                      ]
                      [ Light $ LongRangePointLight (Point (-4) 0 (-7)) (RGB 1 1 1)
                      , Light $ LongRangePointLight (Point 4 0 (-7)) (RGB 1 1 1)
                      ]
    return $ Scene world camera

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
    coloryImage <- render gen rayTracer <$> coloryScene
    display coloryImage
