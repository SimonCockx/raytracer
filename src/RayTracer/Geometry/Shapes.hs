module RayTracer.Geometry.Shapes
    ( createBox
    , Plane (..)
    , Rectangle (..)
    , createRectangle
    , Disc (..)
    , createDisc
    , Sphere (..)
    , createSphere
    , OpenCylinder (..)
    , createOpenCylinder
    , createCylinder
    , createTriangle
    , readObjFileAsList
    , readObjFile
    ) where

import RayTracer.Geometry.Shape
import RayTracer.Geometry.Vector
import RayTracer.Geometry.Transformation
import RayTracer.Geometry.Ray

import Safe
import Text.Read
import Data.List.Split


createBox :: Double -> Double -> Double -> Transformed AABB
createBox x y z = Transformed (scale x y z) $ createAABB (pure (-0.5)) $ pure 0.5


data Plane = Plane !(Point Double) !(Vector Double)
    deriving (Show)

instance Shape Plane where
    intersect ray (Plane p n)
        | t < 0     = Nothing
        | otherwise = Just (t, n, follow ray t <-> p)
        where
            lProj = l <.> n
            dp = p <-> o
            dpProj = dp <.> n
            t = dpProj / lProj
            l = direction ray
            o = origin ray
    boundingBox _ = spaceAABB
    boundedNode = UnboundedNode
instance Transformable Plane Double where
    transform tr (Plane p n) = Plane (transform tr p) (normalTransform tr n)


data Rectangle = Rectangle
    deriving (Show)

instance Shape Rectangle where
    intersect ray _
        | t < 0 || abs pz > 0.5 || abs py > 0.5 = Nothing
        | otherwise                             = Just (t, Vector 1 0 0, Vector (0.5 + pz) (0.5 + py) 0)
        where
            t = -ox/lx
            Point _ py pz = follow ray t
            Vector lx _ _ = direction ray
            Point ox _ _ = origin ray
    boundingBox _ = createAABB (Point 0 (-0.5) (-0.5)) (Point 0 0.5 0.5)
    boundedNode = UnboundedNode

createRectangle :: Vector Double -> Double -> Double -> Transformed Rectangle
createRectangle n width height = Transformed (alignWithXAxis n `inverseTransform` scale 1 height width) Rectangle


data Disc = Disc
    deriving (Show)

instance Shape Disc where
    intersect ray _
        | t < 0 || py*py + pz*pz > 1 = Nothing
        | otherwise                  = Just (t, Vector 1 0 0, Vector pz py 0)
        where
            t = -ox/lx
            Point _ py pz = follow ray t
            Vector lx _ _ = direction ray
            Point ox _ _ = origin ray
    boundingBox _ = createAABB (Point 0 (-1) (-1)) (Point 0 1 1)
    boundedNode = UnboundedNode

createDisc :: Vector Double -> Double -> Transformed Disc
createDisc n r = Transformed (alignWithXAxis n `inverseTransform` scaleUni r) Disc


-- | A type representing a unit sphere.
data Sphere = Sphere
    deriving (Show)
    
instance Shape Sphere where
    intersect ray Sphere
        | d < 0     = Nothing
        | d == 0    = let t = -b/2/a in if t < 0 then Nothing else let v = toVector $ follow ray t in Just (t, v, v)
        | otherwise =
            let dSqrt = sqrt d
                t1 = (-b - dSqrt)/2/a in
            if t1 >= 0 then let v = toVector $ follow ray t1 in Just (t1, v, v)
            else let t2 = (-b + dSqrt)/2/a in
                if t2 >= 0 then let v = toVector $ follow ray t2 in Just (t2, v, v)
                else Nothing
        where
            a = normSqr l
            b = 2 * (l <.> (toVector o))
            c = normSqr (toVector o) - 1
            d = b*b - 4*a*c
            l = direction ray
            o = origin ray
    boundingBox Sphere = createAABB (pure (-1)) (pure 1)

createSphere :: Double -> Transformed Sphere
createSphere radius = Transformed (scaleUni radius) Sphere


data OpenCylinder = OpenCylinder
    deriving (Show)
    
instance Shape OpenCylinder where
    intersect ray OpenCylinder = surfaceIntersection
        where
            a = yd*yd + zd*zd
            b = 2*(yo*yd + zo*zd)
            c = yo*yo + zo*zo - 1
            d = b*b - 4*a*c
            Point xo yo zo = origin ray
            Vector xd yd zd = direction ray
            getSurfaceNormal t' = let Point _ y z = follow ray t' in Vector 0 y z
            getUvw t' = let Point x y z = follow ray t' in Vector ((atan2 y z + pi)/(2*pi)) (x + 0.5) 0 
            surfaceIntersection
                | d < 0     = Nothing
                | d == 0    = let t = -b/2/a
                                  x = xo + t*xd in if t < 0 || x < -0.5 || x > 0.5 then Nothing else Just (t, getSurfaceNormal t, getUvw t)
                | otherwise =
                    let dSqrt = sqrt d
                        t1 = (-b - dSqrt)/2/a
                        x1 = xo + t1*xd in
                    if t1 >= 0 && -0.5 <= x1 && x1 <= 0.5 then Just (t1, getSurfaceNormal t1, getUvw t1)
                    else let t2 = (-b + dSqrt)/2/a
                             x2 = xo + t2*xd in
                        if t2 >= 0 && -0.5 <= x2 && x2 <= 0.5 then Just (t2, getSurfaceNormal t2, getUvw t2)
                        else Nothing
    boundingBox OpenCylinder = createAABB (Point (-0.5) (-1) (-1)) (Point 0.5 1 1)

createOpenCylinder :: Vector Double -> Double -> Double -> Transformed OpenCylinder
createOpenCylinder axis radius l = Transformed (alignWithXAxis axis `inverseTransform` scale l radius radius) OpenCylinder

createCylinder :: Vector Double -> Double -> Double -> Transformed [ShapeWrapper]
createCylinder axis radius l = Transformed (alignWithXAxis axis `inverseTransform` scale l radius radius)
    [ ShapeWrapper OpenCylinder
    , ShapeWrapper $ translate (0.5::Double) 0 0 `transform` createDisc (Vector 1 0 0) 1
    , ShapeWrapper $ translate (-0.5::Double) 0 0 `transform` createDisc (Vector (-1) 0 0) 1
    ]



intersectTriangle :: Ray Double -> Point Double -> Point Double -> Point Double -> Vector Double -> Vector Double -> Vector Double -> Maybe (Double, Double, Double, Double)
intersectTriangle ray p0 _ _ normal betaVector gammaVector
    | 0 <= alpha && alpha <= 1 && 0 <= beta && beta <= 1 && 0 <= gamma && gamma <= 1 = if t < 0 then Nothing else Just (t, alpha, beta, gamma)
    | otherwise = Nothing
    where
        t = (normal <.> (p0 <-> o))/(normal <.> l)
        p = follow ray t
        d = p <-> p0
        beta = d <.> betaVector
        gamma = d <.> gammaVector
        alpha = 1 - beta - gamma
        o = origin ray
        l = direction ray

data Triangle = Triangle !(Point Double) !(Point Double) !(Point Double) !(Vector Double) !(Vector Double) !(Vector Double)
    deriving (Show)
createTriangle :: (Point Double) -> (Point Double) -> (Point Double) -> Triangle
createTriangle p0 p1 p2 = Triangle p0 p1 p2 n betaVector gammaVector
    where
        (e0, e1) = (p1 <-> p0, p2 <-> p0)
        n = normalize $ e0 `cross` e1
        betaVector = ((normSqr e1 *^ e0) ^-^ (e0<.>e1 *^ e1)) ^/ (normSqr e0 * normSqr e1 - (e0 <.> e1)^(2 :: Int))
        gammaVector = ((e0<.>e1 *^ e0) ^-^ (normSqr e0 *^ e1)) ^/ ((e0 <.> e1)^(2 :: Int) - normSqr e0 * normSqr e1)
    
instance Shape Triangle where
    intersect ray (Triangle p0 p1 p2 n bV cV) = do
        (t, alpha, beta, gamma) <- intersectTriangle ray p0 p1 p2 n bV cV
        return (t, n, Vector alpha beta gamma)
    boundingBox (Triangle p0 p1 p2 _ _ _) = getAABB [p0, p1, p2]
instance Transformable Triangle Double where
    transform t (Triangle p0 p1 p2 _ _ _) = createTriangle (transform t p0) (transform t p1) (transform t p2)


data Vertex = Vertex !(Point Double) !(Vector Double) !(Vector Double)
    deriving (Show)

data MeshTriangle = MeshTriangle !Vertex !Vertex !Vertex !(Vector Double) !(Vector Double) !(Vector Double)
    deriving (Show)
createMeshTriangle :: Vertex -> Vertex -> Vertex -> MeshTriangle
createMeshTriangle v0 v1 v2 = MeshTriangle v0 v1 v2 n betaVector gammaVector
    where
        Vertex p0 _ _ = v0
        Vertex p1 _ _ = v1
        Vertex p2 _ _ = v2
        (e0, e1) = (p1 <-> p0, p2 <-> p0)
        n = normalize $ e0 `cross` e1
        betaVector = ((normSqr e1 *^ e0) ^-^ (e0<.>e1 *^ e1)) ^/ (normSqr e0 * normSqr e1 - (e0 <.> e1)^(2 :: Int))
        gammaVector = ((e0<.>e1 *^ e0) ^-^ (normSqr e0 *^ e1)) ^/ ((e0 <.> e1)^(2 :: Int) - normSqr e0 * normSqr e1)
toTriangle :: MeshTriangle -> Triangle
toTriangle (MeshTriangle (Vertex p0 _ _) (Vertex p1 _ _) (Vertex p2 _ _) n aV bV) = Triangle p0 p1 p2 n aV bV

instance Shape MeshTriangle where
    intersect ray (MeshTriangle (Vertex p0 t0 n0) (Vertex p1 t1 n1) (Vertex p2 t2 n2) n bV cV) = do
        (t, alpha, beta, gamma) <- intersectTriangle ray p0 p1 p2 n bV cV
        return (t, alpha*^n0 ^+^ beta*^n1 ^+^ gamma*^n2, alpha*^t0 ^+^ beta*^t1 ^+^ gamma*^t2)
    boundingBox (MeshTriangle (Vertex p0 _ _) (Vertex p1 _ _) (Vertex p2 _ _) _ _ _) = getAABB [p0, p1, p2]
instance Transformable MeshTriangle Double where
    transform t (MeshTriangle (Vertex p0 t0 n0) (Vertex p1 t1 n1) (Vertex p2 t2 n2) _ _ _) =
        createMeshTriangle (Vertex (transform t p0) t0 (normalTransform t n0)) (Vertex (transform t p1) t1 (normalTransform t n1)) (Vertex (transform t p2) t2 (normalTransform t n2))


readPoint :: String -> Maybe (Point Double)
readPoint s =
    case ws of
        ["v", x, y, z] -> mapM readMaybe $ Point x y z
        ["v", x, y, z, w] -> do
            [x', y', z', w'] <- mapM readMaybe [x, y, z, w]
            return $ Point (x'/w') (y'/w') (z'/w')
        _ -> Nothing
    where
        ws = words s

readTextureCoordinate :: String -> Maybe (Vector Double)
readTextureCoordinate s =
    case ws of
        ["vt", u] -> do
            u' <- readMaybe u
            return $ Vector u' 0 0
        ["vt", u, v] -> do
            [u', v'] <- mapM readMaybe [u, v]
            return $ Vector u' v' 0
        ["vt", u, v, w] -> mapM readMaybe $ Vector u v w
        _ -> Nothing
    where
        ws = words s

readNormal :: String -> Maybe (Vector Double)
readNormal s =
    case ws of
        ["vn", x, y, z] -> normalize <$> mapM readMaybe (Vector x y z)
        _ -> Nothing
    where
        ws = words s

readMeshTriangles :: [Point Double] -> [Vector Double] -> [Vector Double] -> String -> Maybe [MeshTriangle]
readMeshTriangles vs ts ns s = let ws = words s in
    case ws of
        ["f", p0, p1, p2] -> do
            v0 <- parseVertex p0
            v1 <- parseVertex p1
            v2 <- parseVertex p2
            return $ [createMeshTriangle v0 v1 v2]
        ["f", p0, p1, p2, p3] -> do
            v0 <- parseVertex p0
            v1 <- parseVertex p1
            v2 <- parseVertex p2
            v3 <- parseVertex p3
            return $ [createMeshTriangle v0 v1 v2, createMeshTriangle v0 v2 v3]
        _ -> Nothing
    where
        parseVertex p = let ps = splitOn "/" p in
            case ps of
                [vi, "", ni] -> do
                    v <- (+(-1)) <$> readMaybe vi >>= atMay vs
                    n <- (+(-1)) <$> readMaybe ni >>= atMay ns
                    return $ Vertex v (Vector 0 0 0 ) n
                [vi, ti, ni] -> do
                    v <- (+(-1)) <$> readMaybe vi >>= atMay vs
                    t <- (+(-1)) <$> readMaybe ti >>= atMay ts
                    n <- (+(-1)) <$> readMaybe ni >>= atMay ns
                    return $ Vertex v t n
                _ -> Nothing

readIgnored :: String -> Maybe ()
readIgnored s = let ws = words s in
    case ws of
        ('#':_):_ -> Just () -- comments

        "o":_ -> Just ()
        "g":_ -> Just ()
        "s":_ -> Just ()
        "usemtl":_ -> Just ()
        "mtllib":_ -> Just ()
        [] -> Just () -- empty lines

        _ -> Nothing

readTriangleMesh :: String -> Maybe [MeshTriangle]
readTriangleMesh s = readMesh [] [] [] [] $ lines s
    where
        readMesh _ _ _ triangles [] = Just triangles
        readMesh vs ts ns triangles (line:ls) = case readPoint line of
                Just v -> readMesh (vs ++ [v]) ts ns triangles ls
                Nothing -> case readTextureCoordinate line of
                    Just t -> readMesh vs (ts ++ [t]) ns triangles ls
                    Nothing -> case readNormal line of
                        Just n -> readMesh vs ts (ns ++ [n]) triangles ls
                        Nothing -> case readMeshTriangles vs ts ns line of
                            Just trs -> readMesh vs ts ns (trs ++ triangles) ls
                            Nothing -> case readIgnored line of
                                Just () -> readMesh vs ts ns triangles ls
                                Nothing -> Nothing

readObjFileAsList :: FilePath -> IO [Triangle]
readObjFileAsList = fmap (map toTriangle) . readObjFile

readObjFile :: FilePath -> IO [MeshTriangle]
readObjFile p = do
    f <- readFile p
    case readTriangleMesh f of
        Nothing -> fail $ "Failed to read file " ++ p
        Just triangles -> return triangles


