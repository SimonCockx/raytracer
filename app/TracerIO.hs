module TracerIO where

import RayTracer
import System.IO
import qualified Data.Massiv.Array as A
import Data.List.Split (splitOn)

saveSpectralImage :: (Show s, A.Mutable r A.Ix2 s) => FilePath -> SpectralImageR r s -> IO ()
saveSpectralImage path arr = do
  let A.Sz (rows A.:. columns) = A.size arr
  handle <- openFile path WriteMode
  hPutStr handle $ show rows
  hPutChar handle ';'
  hPutStr handle $ show columns
  A.foldlS (\acc spec -> acc >> hPutChar handle ';' >> hPutStr handle (show spec)) mempty arr
  hClose handle

readSpectralImage :: (Read s, A.Mutable r A.Ix2 s) => FilePath -> IO (SpectralImageR r s)
readSpectralImage path = do
  f <- readFile path
  let _ : columnsS : valuesS = splitOn ";" f
      -- rows = read rowsS
      columns = read columnsS
      values = map read valuesS
  A.fromListsM A.Par $ toMatrix columns columns values
    where
      toMatrix :: Int -> Int -> [a] -> [[a]]
      toMatrix 1 _ [v] = [[v]]
      toMatrix 0 n vs = [] : toMatrix n n vs
      toMatrix i n (v:vs) = let rest = toMatrix (i-1) n vs in (v : head rest) : tail rest
      toMatrix _ _ [] = error "dimensions did not match"
