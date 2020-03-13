module RayTracer.Core.Sampling
    ( SamplingStrategy (..)
    , sample
    ) where

import RayTracer.Random


data SamplingStrategy 
    = RegularGrid Int
    | Random Int
    | Stratified Int
    deriving (Show)

sample :: (MonadRandom m) => SamplingStrategy -> m [(Double, Double)]
sample strategy = case strategy of
    RegularGrid n -> return $ points
        where
            offset = 1/(2*(fromIntegral n))-0.5
            points = [(offset + ip, offset + jp) | i <- [0..(n-1)], j <- [0..(n-1)], let ip = (fromIntegral i) / (fromIntegral n), let jp = (fromIntegral j) / (fromIntegral n)]
    Random n -> replicateM n $ do
                    x <- getRandomR (-0.5, 0.5)
                    y <- getRandomR (-0.5, 0.5)
                    return (x, y)
