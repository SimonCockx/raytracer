module RayTracer.Core.Sampling
    ( SamplingStrategy (..)
    , getSample
    ) where

import RayTracer.Random
import Control.Monad (replicateM)


data SamplingStrategy 
    = RegularGrid Int
    | Random Int
    | Stratified Int
    deriving (Show)

getSample :: (MonadRandom m) => SamplingStrategy -> Double -> Double -> Double -> Double -> m [(Double, Double)]
getSample strategy centerX centerY width height =
  case strategy of
    RegularGrid n -> return points
      where offsetX = centerX + width * (1 / (2 * fromIntegral n) - 0.5)
            offsetY = centerY + height * (1 / (2 * fromIntegral n) - 0.5)
            points =
              [ (offsetX + width * ip, offsetY + height * jp)
              | i <- [0 .. (n - 1)]
              , j <- [0 .. (n - 1)]
              , let ip = fromIntegral i / fromIntegral n
              , let jp = fromIntegral j / fromIntegral n
              ]
    Random n ->
      replicateM n $ do
        x <- (centerX +) <$> getRandomR (-width / 2, width / 2)
        y <- (centerY +) <$> getRandomR (-height / 2, height / 2)
        return (x, y)
    Stratified n ->
      let stratumX = width / fromIntegral n
          stratumY = height / fromIntegral n
       in do grid <- getSample (RegularGrid n) centerX centerY width height
             offsets <- getSample (Random $ n * n) 0 0 stratumX stratumY
             return $ zipWith (\(cx, cy) (ox, oy) -> (cx + ox, cy + oy)) grid offsets
{-# INLINE getSample #-}
