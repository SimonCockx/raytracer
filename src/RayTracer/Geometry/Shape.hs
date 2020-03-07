module RayTracer.Geometry.Shape
    ( Shape (..)
    , closest
    , Intersection
    , closestIntersection
    , TransformedShape (..)
    , Sphere (..)
    , createSphere
    , AACube (..)
    , createAABox
    , Cylinder (..)
    , createCylinder
    , createTriangle
    , readObjFileAsList
    , readObjFile
    ) where

import RayTracer.Geometry.Vector
import RayTracer.Geometry.Ray
import RayTracer.Geometry.Transformation
import Data.Maybe
import Control.Monad
import Text.Read
import Data.List.Split
import Safe

type Intersection = (Double, Vector Double)

closest :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a
closest f = maybeC (\x y -> if f x < f y then x else y)
    where
        maybeC :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
        maybeC f Nothing Nothing = Nothing
        maybeC f (Just x) Nothing = Just x
        maybeC f Nothing (Just y) = Just y
        maybeC f (Just x) (Just y) = Just $ f x y
{-# INLINE closest #-}

closestIntersection :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
closestIntersection = closest fst
{-# INLINE closestIntersection #-}

-- | A class representing a shape that can be intersected by a ray.
class Shape a where
    -- | Compute the closest intersection of a ray with this shape, if any.
    intersect :: Ray Double -> a -> Maybe Intersection

instance (Shape a) => Shape [a] where
    intersect ray shapes = do
        (t, n) <- foldr (closestIntersection . intersect ray) Nothing shapes
        return (t, n)


data TransformedShape a = Transformed (Transformation Double) a
instance (Shape a) => Transformable (TransformedShape a) Double where
    transform t (Transformed t' s) = Transformed (t `transform` t') s

instance (Show a) => Show (TransformedShape a) where
    show (Transformed t s) = "Transformed (" ++ show t ++ ") (" ++ show s ++ ")"

instance (Shape a) => Shape (TransformedShape a) where
    intersect ray (Transformed m s) = do
        (t, n) <- intersect (inverseTransform m ray) s
        return (t, normalTransform m n)


-- | A type representing a unit sphere.
data Sphere = Sphere
    deriving (Show)

instance Shape Sphere where
    intersect ray Sphere
        | d < 0     = Nothing
        | d == 0    = let t = -b/2/a in if t < 0 then Nothing else Just (t, toVector $ follow ray t)
        | otherwise =
            let dSqrt = sqrt d
                t1 = (-b - dSqrt)/2/a in
            if t1 >= 0 then Just (t1, toVector $ follow ray t1)
            else let t2 = (-b + dSqrt)/2/a in
                if t2 >= 0 then Just (t2, toVector $ follow ray t2)
                else Nothing
        where
            a = normSqr l
            b = 2 * (l <.> (toVector o))
            c = normSqr (toVector o) - 1
            d = b*b - 4*a*c
            l = direction ray
            o = origin ray

createSphere :: Double -> TransformedShape Sphere
createSphere radius = Transformed (scaleUni radius) Sphere



data AACube = AACube
    deriving (Show)
instance Shape AACube where
    intersect ray AACube
        | tmax < 0 || tmin > tmax = Nothing
        | otherwise               = Just (tmin, normal)
        where
            tx0 = (0.5-xo)/xd
            tx1 = (-0.5-xo)/xd
            ty0 = (0.5-yo)/yd
            ty1 = (-0.5-yo)/yd
            tz0 = (0.5-zo)/zd
            tz1 = (-0.5-zo)/zd
            tmin = max (max (min tx0 tx1) (min ty0 ty1)) (min tz0 tz1)
            normal = if tmin == tx0 then Vector 1 0 0
                else if tmin == tx1 then Vector (-1) 0 0
                else if tmin == ty0 then Vector 0 1 0
                else if tmin == ty1 then Vector 0 (-1) 0
                else if tmin == tz0 then Vector 0 0 1
                else Vector 0 0 (-1)
            tmax = min (min (max tx0 tx1) (max ty0 ty1)) (max tz0 tz1)
            Point xo yo zo = origin ray
            Vector xd yd zd = direction ray

createAABox :: Double -> Double -> Double -> TransformedShape AACube
createAABox xSize ySize zSize = Transformed (scale xSize ySize zSize) AACube



data Cylinder = Cylinder
    deriving (Show)
instance Shape Cylinder where
    intersect ray Cylinder = closestIntersection upperIntersection $ closestIntersection lowerIntersection surfaceIntersection
        where
            a = xd*xd + zd*zd
            b = 2*(xo*xd + zo*zd)
            c = xo*xo + zo*zo - 1
            d = b*b - 4*a*c
            tUp = (0.5 - yo)/yd
            xUp = xo + tUp*xd
            zUp = zo + tUp*zd
            tDown = (-0.5 - yo)/yd
            xDown = xo + tDown*xd
            zDown = zo + tDown*zd
            Point xo yo zo = origin ray
            Vector xd yd zd = direction ray
            upperIntersection
                | tUp < 0 || xUp*xUp + zUp*zUp > 1 = Nothing
                | otherwise                        = Just (tUp, Vector 0 1 0)
            lowerIntersection
                | tDown < 0 || xDown*xDown + zDown*zDown > 1 = Nothing
                | otherwise                                  = Just (tDown, Vector 0 (-1) 0)
            getSurfaceNormal t' = let Point x y z = follow ray t' in normalize $ Vector x 0 z
            surfaceIntersection
                | d < 0     = Nothing
                | d == 0    = let t = -b/2/a
                                  y = yo + t*yd in if t < 0 || y < -0.5 || y > 0.5 then Nothing else Just (t, getSurfaceNormal t)
                | otherwise =
                    let dSqrt = sqrt d
                        t1 = (-b - dSqrt)/2/a
                        y1 = yo + t1*yd in
                    if t1 >= 0 && -0.5 <= y1 && y1 <= 0.5 then Just (t1, getSurfaceNormal t1)
                    else let t2 = (-b + dSqrt)/2/a
                             y2 = yo + t2*yd in
                        if t2 >= 0 && -0.5 <= y2 && y2 <= 0.5 then Just (t2, getSurfaceNormal t2)
                        else Nothing

createCylinder :: Double -> Double -> TransformedShape Cylinder
createCylinder radius length = Transformed (scale radius length radius) Cylinder



intersectTriangle ray p0 p1 p2 normal betaVector gammaVector
    | 0 <= alpha && alpha <= 1 && 0 <= beta && beta <= 1 && 0 <= gamma && gamma <= 1 = if t < 0 then Nothing else Just (t, alpha, beta, gamma)
    | otherwise = Nothing
    where
        ed0 = p1 <-> p0
        ed2 = p0 <-> p2
        t = (normal <.> (p0 <-> o))/(normal <.> l)
        p = follow ray t
        d = p <-> p0
        beta = d <.> betaVector
        gamma = d <.> gammaVector
        alpha = 1 - beta - gamma
        c0 = p <-> p0
        c1 = p <-> p1
        c2 = p <-> p2
        o = origin ray
        l = direction ray

data Triangle = Triangle (Point Double) (Point Double) (Point Double) (Vector Double) (Vector Double) (Vector Double)
    deriving (Show)
createTriangle :: (Point Double) -> (Point Double) -> (Point Double) -> Triangle
createTriangle p0 p1 p2 = Triangle p0 p1 p2 n betaVector gammaVector
    where
        (e0, e1) = (p1 <-> p0, p2 <-> p0)
        n = normalize $ e0 `cross` e1
        betaVector = ((normSqr e1 *^ e0) ^-^ (e0<.>e1 *^ e1)) ^/ (normSqr e0 * normSqr e1 - (e0 <.> e1)^2)
        gammaVector = ((e0<.>e1 *^ e0) ^-^ (normSqr e0 *^ e1)) ^/ ((e0 <.> e1)^2 - normSqr e0 * normSqr e1)
instance Shape Triangle where
    intersect ray (Triangle p0 p1 p2 n bV cV) = do
        (t, _, _, _) <- intersectTriangle ray p0 p1 p2 n bV cV
        return (t, n)


data Vertex = Vertex (Point Double) (Double, Double) (Vector Double)
    deriving (Show)

data MeshTriangle = MeshTriangle Vertex Vertex Vertex (Vector Double) (Vector Double) (Vector Double)
    deriving (Show)
createMeshTriangle :: Vertex -> Vertex -> Vertex -> MeshTriangle
createMeshTriangle v0 v1 v2 = MeshTriangle v0 v1 v2 n betaVector gammaVector
    where
        Vertex p0 _ _ = v0
        Vertex p1 _ _ = v1
        Vertex p2 _ _ = v2
        (e0, e1) = (p1 <-> p0, p2 <-> p0)
        n = normalize $ e0 `cross` e1
        betaVector = ((normSqr e1 *^ e0) ^-^ (e0<.>e1 *^ e1)) ^/ (normSqr e0 * normSqr e1 - (e0 <.> e1)^2)
        gammaVector = ((e0<.>e1 *^ e0) ^-^ (normSqr e0 *^ e1)) ^/ ((e0 <.> e1)^2 - normSqr e0 * normSqr e1)
toTriangle :: MeshTriangle -> Triangle
toTriangle (MeshTriangle (Vertex p0 _ _) (Vertex p1 _ _) (Vertex p2 _ _) n aV bV) = Triangle p0 p1 p2 n aV bV
instance Shape MeshTriangle where
    intersect ray (MeshTriangle (Vertex p0 t0 n0) (Vertex p1 t1 n1) (Vertex p2 t2 n2) n bV cV) = do
        (t, alpha, beta, gamma) <- intersectTriangle ray p0 p1 p2 n bV cV
        return (t, alpha*^n0 ^+^ beta*^n1 ^+^ gamma*^n2)


readPoint :: String -> Maybe (Point Double)
readPoint s =
    case ws of
        ["v", x, y, z] -> do
            x' <- readMaybe x
            y' <- readMaybe y
            z' <- readMaybe z
            return $ Point x' y' z'
        _ -> Nothing
    where
        ws = words s

readTextureCoordinate :: String -> Maybe (Double, Double)
readTextureCoordinate s =
    case ws of
        ["vt", u, v] -> do
            u' <- readMaybe u
            v' <- readMaybe v
            return $ (u', v')
        _ -> Nothing
    where
        ws = words s

readNormal :: String -> Maybe (Vector Double)
readNormal s =
    case ws of
        ["vn", x, y, z] -> do
            x' <- readMaybe x
            y' <- readMaybe y
            z' <- readMaybe z
            return $ normalize $ Vector x' y' z'
        _ -> Nothing
    where
        ws = words s

readMeshTriangle :: [Point Double] -> [(Double, Double)] -> [Vector Double] -> String -> Maybe MeshTriangle
readMeshTriangle vs ts ns s = let ws = words s in
    case ws of
        ["f", p0, p1, p2] -> do
            v0 <- parseVertex p0
            v1 <- parseVertex p1
            v2 <- parseVertex p2
            return $ createMeshTriangle v0 v1 v2
        _ -> Nothing
    where
        parseVertex p = let ps = splitOn "/" p in
            case ps of
                [vi, ti, ni] -> do
                    v <- (+(-1)) <$> readMaybe vi >>= atMay vs
                    t <- (+(-1)) <$> readMaybe ti >>= atMay ts
                    n <- (+(-1)) <$> readMaybe ni >>= atMay ns
                    return $ Vertex v t n
                _ -> Nothing

readTriangleMesh :: String -> Maybe [MeshTriangle]
readTriangleMesh s = readMesh [] [] [] [] $ lines s
    where
        readMesh vs ts ns triangles [] = Just triangles
        readMesh vs ts ns triangles (line:ls) = case readPoint line of
                Just v -> readMesh (vs ++ [v]) ts ns triangles ls
                Nothing -> case readTextureCoordinate line of
                    Just t -> readMesh vs (ts ++ [t]) ns triangles ls
                    Nothing -> case readNormal line of
                        Just n -> readMesh vs ts (ns ++ [n]) triangles ls
                        Nothing -> case readMeshTriangle vs ts ns line of
                            Just triangle -> readMesh vs ts ns (triangles ++ [triangle]) ls
                            Nothing -> case line of
                                "" -> readMesh vs ts ns triangles ls
                                _ -> Nothing

readObjFileAsList :: FilePath -> IO [Triangle]
readObjFileAsList = fmap (map toTriangle) . readObjFile

readObjFile :: FilePath -> IO [MeshTriangle]
readObjFile p = do
    f <- readFile p
    case readTriangleMesh f of
        Nothing -> fail $ "Failed to read file " ++ p
        Just triangles -> return triangles

