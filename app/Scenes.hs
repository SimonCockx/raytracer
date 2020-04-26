module Scenes where

import RayTracer
import Prelude hiding (floor)

import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.RGBSpace (uncurryRGB)

camera1 :: PerspectiveCamera
camera1 = createPerspectiveCamera 600 400 (Point 0 0 0) (Vector 0 0 (-1)) (Vector 0 1 0) (pi/2) (RegularGrid 1)

vlekjeScene :: IO (Scene RGB)
vlekjeScene = do
    teaPotObj <- readObjFile "objects/teaPot.obj"
    vlekje <- readTextureMap "objects/vlekje.png" :: IO (TextureMap RGB)
    let floor = translate 0 (0::Double) 0 `transform` createBox 50 2 50
        left  = translate (-8:: Double) 0 0 `transform` createBox 2 50 50
        right = translate (8::Double) 0 0 `transform` createBox 2 50 50
        back  = translate 0 0 (-16::Double) `transform` createBox 50 50 2
        top = translate 0 (16::Double) 0 `transform` createBox 50 2 50
        teaPot1 = Transformed ((translate (5::Double) 9 (-13)) `transform` (scaleUni (3::Double)) `transform` rotateY (-pi/6::Double)) teaPotObj
        teaPot2 = Transformed ((translate (0::Double) 1 (-10)) `transform` (scaleUni 4)) teaPotObj
        light = translate 0 14.5 (-8::Double) `transform` createAreaLight (Vector 0 1 0) 4 4 (RGB 6 6 6)

        world = createWorld [ withMaterial teaPot1 (DiffuseTexture vlekje (\(Vector u v w) -> (u, v)))
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
    return $ Scene (insertBoundingBoxes world) camera1

textureMapScene :: IO (Scene RGB)
textureMapScene = do
    appleObj <- readObjFile "objects/apple/apple.obj"
    appleTex <- readTextureMap "objects/apple/apple_texture.jpg" :: IO (TextureMap RGB)
    let appleShape = translate 0 (-2) (-4::Double) `transform` appleObj
        apple = withMaterial appleShape $ DiffuseTexture appleTex (\(Vector u v w) -> (u, v))
        sphere1 = translate 3 0 (-5::Double) `transform` rotateZ (pi/6::Double) `transform` (createSphere 1)
        rainbowSphere1 = withMaterial sphere1 $ ProceduralDiffuseTexture (\(Vector u v w) -> let a = 360/pi*acos v in uncurryRGB RGB $ hsl a 1 0.5)
        sphere2 = translate (-2) (-0.5) (-5::Double) `transform` rotateZ (-pi/3::Double) `transform` (createSphere 1)
        rainbowSphere2 = withMaterial sphere2 $ ProceduralDiffuseTexture (\(Vector u v w) -> let a = 180*(v + 1) in uncurryRGB RGB $ hsl a 1 0.5)
        sphere3 = translate 2 (-2.5) (-4.5::Double) `transform` rotateZ (0::Double) `transform` (createSphere 1)
        strange = withMaterial sphere3 $ ProceduralDiffuseTexture (\(Vector u v w) -> RGB (sin (10*u*w) + 1/(3+ acos v)**2) (abs v/(1 + cos (20/(w + v**2)) **2)) (exp (u + w - sqrt 2)))
        world = createWorld [ apple
                            , rainbowSphere1
                            , rainbowSphere2
                            , strange
                            ]
                            [ Light $ AmbientLight $ RGB 2.4 2.4 2.4
                            ]
    return $ Scene (insertBoundingBoxes world) camera1

tilesScene :: IO (Scene RGB)
tilesScene = do
    let size = 3
        xCount = 100
        zCount = 200
        tiles = map (\(x, z) -> (`withMaterial` (Diffuse (fromIntegral (round (x+z) `mod` 2) *^ RGB 1 1 1))) $ translate ((x-(xCount-1)/2)*size) (-1.005) (-z*size+size/2) `transform` createBox size 0.01 size) $ [(a, b) | a <- [0..(xCount-1)], b <- [0..(zCount-1)]]

        world = createWorld tiles
                            [ Light $ AmbientLight $ RGB 2.4 2.4 2.4
                            ]
    return $ Scene (insertBoundingBoxes world) camera1

softShadowScene :: IO (Scene Gray)
softShadowScene = do
    let floor = translate (-2) (-3) (-9::Double) `transform` createBox 20 2 20
        wall = (translate (-0.5::Double) (-0) (-6.5)) `transform` createBox 0.1 4 4
        light = translate 2.5 1 (-6.5::Double) `transform` createAreaLight (Vector 1 1 0) 4.5 4.5 (Gray 3)

        world = createWorld [ simpleObject wall
                            , simpleObject floor
                            , SceneLight light
                            ]
                            [ Light light
                            ]
    return $ Scene (insertBoundingBoxes world) camera1

sphereLineScene :: IO (Scene RGB)
sphereLineScene = do
    let n = 8
        spheres = map (\a -> translate (-0.9) (-0.9) (-a-3::Double) `transform` createSphere 1)
            [i | i <- [0..(n-1)]]

        world = createWorld [ simpleObject spheres
                      ]
                      [ Light $ LongRangePointLight (Point 2 0 (-5)) (RGB 6 6 6)
                      , Light $ LongRangePointLight (Point 0 0 0) (RGB 6 6 6)
                      ]
        acceleratedWorld = insertBoundingBoxes world
    return $ Scene acceleratedWorld camera1

shapeScene :: IO (Scene Gray)
shapeScene = do
    icosahedron <- Transformed (translate 4 1 (-9::Double) `transform` (scaleUni 3)) <$> readObjFile "objects/icosahedron.obj"
    let elipse   = translate 0 (-2) (-10::Double) `transform` (rotateZ (pi/4 :: Double)) `transform` (scale (1 :: Double) 2 1) `transform` (createSphere 1.5)
        sphere   = translate (-2) 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-2) (-9::Double) `transform` createBox 0.5 1 5
        cylinder = translate 7 (-5) (-10::Double) `transform` (rotateZ (pi/18 :: Double)) `transform` createCylinder (Vector 0 1 0) 1 3
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))

        world    = createWorld [ simpleObject icosahedron
                         , simpleObject elipse
                         , simpleObject sphere
                         , simpleObject box
                         , simpleObject cylinder
                         , simpleObject triangle
                         ]
                         [ Light $ LongRangePointLight (Point (-4) 0 (-3)) $ 6 *^ (white :: Gray)
                         , Light $ LongRangePointLight (Point 4 0 (-3)) $ 6 *^ (white :: Gray)
                         ]
    return $ Scene world camera1

phongScene :: IO (Scene Gray)
phongScene = do
    teaPot <- readObjFile "objects/teaPot.obj"
    teaPotList <- readObjFileAsList "objects/teaPot.obj"
    let teaPot1  = Transformed ((translate (-4::Double) (-2) (-9)) `transform` (scaleUni 3.5)) teaPot
        teaPot2  = Transformed ((translate (4::Double) (-2) (-9)) `transform` (scaleUni 3.5)) teaPotList

        world = createWorld [ simpleObject teaPot1
                      , simpleObject teaPot2
                      ]
                      [ Light $ LongRangePointLight (Point (-4) 1 (-5)) $ 9 *^ (white :: Gray)
                      , Light $ LongRangePointLight (Point 4 1 (-5)) $ 9 *^ (white :: Gray)
                      ]
    return $ Scene world camera1

lightningScene :: IO (Scene RGB)
lightningScene = do
    icosahedron <- Transformed ((translate (4::Double) 1 (-10)) `transform` (scaleUni 3)) <$> readObjFile "objects/icosahedron.obj"
    let sphere   = translate (-2) 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-1) (-9::Double) `transform` createBox 0.5 1 5
        cylinder = translate 6 (-2) (-8::Double) `transform` createCylinder (Vector 0 1 0) 1 3
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))
        floor    = translate 0 (-3) (-9::Double) `transform` createBox 30 2 30

        world = createWorld [ withMaterial icosahedron $ Diffuse $ RGB 0.4 0 0.1
                      , withMaterial sphere $ Diffuse $ RGB 1 1 0
                      , simpleObject box
                      , simpleObject cylinder
                      , simpleObject triangle
                      , simpleObject floor
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 6 3 3)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 3 6 3)
                      , Light $ LongRangePointLight (Point 4.5 3 (-5)) (RGB 3 3 6)
                      , Light $ LongRangePointLight (Point (-3) (-3) (-9)) (RGB 3 6 3)
                      , Light $ LongRangePointLight (Point 0 (-3) (-9)) (RGB 3 3 6)
                      , Light $ LongRangePointLight (Point 5 (-3) (-9)) (RGB 6 3 3)
                      ]
    return $ Scene world camera1

coloryScene :: IO (Scene RGB)
coloryScene = do
    icosahedron <- Transformed (translate 4 1 (-9::Double) `transform` scaleUni 3) <$> readObjFile "objects/icosahedron.obj"
    let elipse   = translate 0 (-2) (-10::Double) `transform` (rotateZ (pi/4 :: Double)) `transform` (scale (1 :: Double) 2 1) `transform` (createSphere 1.5)
        sphere   = translate 2 0.5 (-11::Double) `transform` createSphere 1.5
        box      = translate (-5) (-2) (-9::Double) `transform` createBox 0.5 1 5
        cylinder = translate 7 (-5) (-10::Double) `transform` createCylinder (Vector 0 1 0) 2 4
        triangle = createTriangle (Point (-8) 0 (-8)) (Point (-3) 2 (-8)) (Point (-6) 5 (-8))

        world = createWorld [ simpleObject icosahedron
                      , simpleObject elipse
                      , simpleObject sphere
                      , simpleObject box
                      , simpleObject cylinder
                      , simpleObject triangle
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 9 3 3)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 3 9 3)
                      , Light $ LongRangePointLight (Point 4.5 3 (-5)) (RGB 3 3 15)
                      , Light $ LongRangePointLight (Point (-3) (-3) (-9)) (RGB 3 9 3)
                      , Light $ LongRangePointLight (Point 0 (-3) (-9)) (RGB 3 3 15)
                      , Light $ LongRangePointLight (Point 5 (-3) (-9)) (RGB 9 3 3)
                      ]
    return $ Scene world camera1

lonelyTeapot :: IO (Scene RGB)
lonelyTeapot = do
    teaPot   <- Transformed ((translate (-2::Double) (-2) (-9)) `transform` (scaleUni 2)) <$> readObjFile "objects/teaPot.obj"
    let floor = translate (-2) (-3) (-9::Double) `transform` createBox 20 2 20

        world = createWorld [ withMaterial teaPot (Diffuse $ RGB 0.8 0.5 0.9)
                      , simpleObject floor
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 9 3 3)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 3 9 3)
                      ]
    return $ Scene world camera1

siyunScene :: IO (Scene RGB)
siyunScene = do
    planeObj   <- readObjFile "objects/plane-siyun.obj"
    let floor = translate (-2) (-5) (-9::Double) `transform` createBox 20 2 20
        plane = translate 0 (-2) (-13::Double) `transform` rotateY (pi/6::Double) `transform` rotateX (pi/6::Double) `transform` scaleUni (0.03::Double) `transform` planeObj

        world = createWorld [ withMaterial plane (Diffuse $ RGB 0 0.8 0)
                      , simpleObject floor
                      ]
                      [ Light $ LongRangePointLight (Point (-2) 1 (-6)) (RGB 3 3 3)
                      , Light $ LongRangePointLight (Point 0 3 (-9)) (RGB 3 3 3)
                      ]
    return $ Scene (insertBoundingBoxes world) camera1
