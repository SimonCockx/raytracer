module Scenes where

import RayTracer

camera1 :: PerspectiveCamera
camera1 = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)

shapeScene :: IO (Scene Gray)
shapeScene = do
    icosahedron <- Transformed (translate 4 1 (-9::Double) `transform` (scaleUni 3)) <$> readObjFile "objects/icosahedron.obj"
    let elipse   = translate 0 (-2) (-10::Double) `transform` (rotateZ (pi/4 :: Double)) `transform` (scale (1 :: Double) 2 1) `transform` (createSphere 1.5)
        sphere   = translate (-2) 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-2) (-9::Double) `transform` createAABox 0.5 1 5
        cylinder = translate 7 (-5) (-10::Double) `transform` (rotateZ (pi/18 :: Double)) `transform` createCylinder 1 3
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))

        world    = World [ simpleObject icosahedron
                         , simpleObject elipse
                         , simpleObject sphere
                         , simpleObject box
                         , simpleObject cylinder
                         , simpleObject triangle
                         ]
                         [ Light $ LongRangePointLight (Point (-4) 0 (-3)) $ 2 *^ (white :: Gray)
                         , Light $ LongRangePointLight (Point 4 0 (-3)) $ 2 *^ (white :: Gray)
                         ]
    return $ Scene world camera1

phongScene :: IO (Scene Gray)
phongScene = do
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
    return $ Scene world camera1

lightningScene :: IO (Scene RGB)
lightningScene = do
    icosahedron <- Transformed ((translate (4::Double) 1 (-10)) `transform` (scaleUni 3)) <$> readObjFile "objects/icosahedron.obj"
    let sphere   = translate (-2) 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-1) (-9::Double) `transform` createAABox 0.5 1 5
        cylinder = translate 6 (-2) (-8::Double) `transform` createCylinder 1 3
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))
        floor    = translate 0 (-3) (-9::Double) `transform` createAABox 30 2 30

        world = World [ SceneObject icosahedron $ Diffuse $ RGB 0.4 0 0.1
                      , SceneObject sphere $ Diffuse $ RGB 1 1 0
                      , simpleObject box
                      , simpleObject cylinder
                      , simpleObject triangle
                      , simpleObject floor
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 2 1 1)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 1 2 1)
                      , Light $ LongRangePointLight (Point 4.5 3 (-5)) (RGB 1 1 2)
                      , Light $ LongRangePointLight (Point (-3) (-3) (-9)) (RGB 1 2 1)
                      , Light $ LongRangePointLight (Point 0 (-3) (-9)) (RGB 1 1 2)
                      , Light $ LongRangePointLight (Point 5 (-3) (-9)) (RGB 2 1 1)
                      ]
    return $ Scene world camera1

coloryScene :: IO (Scene RGB)
coloryScene = do
    icosahedron <- Transformed (translate 4 1 (-9::Double) `transform` scaleUni 3) <$> readObjFile "objects/icosahedron.obj"
    let elipse   = translate 0 (-2) (-10::Double) `transform` (rotateZ (pi/4 :: Double)) `transform` (scale (1 :: Double) 2 1) `transform` (createSphere 1.5)
        sphere   = translate 2 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-2) (-9::Double) `transform` createAABox 0.5 1 5
        cylinder = translate 7 (-5) (-10::Double) `transform` createCylinder 1 3
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))

        world = World [ simpleObject icosahedron
                      , simpleObject elipse
                      , simpleObject sphere
                      , simpleObject box
                      , simpleObject cylinder
                      , simpleObject triangle
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 3 1 1)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 1 3 1)
                      , Light $ LongRangePointLight (Point 4.5 3 (-5)) (RGB 1 1 5)
                      , Light $ LongRangePointLight (Point (-3) (-3) (-9)) (RGB 1 3 1)
                      , Light $ LongRangePointLight (Point 0 (-3) (-9)) (RGB 1 1 5)
                      , Light $ LongRangePointLight (Point 5 (-3) (-9)) (RGB 3 1 1)
                      ]
    return $ Scene world camera1

lonelyTeapot :: IO (Scene RGB)
lonelyTeapot = do
    teaPot   <- Transformed ((translate (-2::Double) (-2) (-9)) `transform` (scaleUni 2)) <$> readObjFile "objects/teaPot.obj"
    let floor = translate (-2) (-3) (-9::Double) `transform` createAABox 20 2 20

        world = World [ SceneObject teaPot (Diffuse $ RGB 0.8 0.5 0.9)
                      , simpleObject floor
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 3 1 1)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 1 3 1)
                      ]
    return $ Scene world camera1
