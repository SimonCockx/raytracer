{-# LANGUAGE FlexibleContexts #-}

module RayTracer.Random 
    ( MonadRunner (..)
    , Gen
    , createGen
    , createGenArray
    --, createRandomArray
    , sequenceRand
    , getWorkerStates
    , module System.Random
    , MonadRandom (..)
    , module Control.Monad.Random.Lazy
    ) where

import System.Random
import Control.Monad.Random
import Control.Monad.Random.Lazy
import System.Random.SplitMix
import Data.Massiv.Array hiding (map)
import qualified Data.Massiv.Array as A
import Data.Word (Word64)

import Data.Massiv.Core
import Control.Scheduler

class MonadRunner g m where
    run :: m a -> g -> (a, g)

instance MonadRunner g (Rand g) where
    run = runRand

type Gen = SMGen

createGen :: Word64 -> Gen
createGen = mkSMGen

createGenArray :: (MonadSplit g m, RandomGen g, Index ix) => Comp -> Sz ix -> m (Array B ix g)
createGenArray comp sz = do
    gen <- getSplit
    let genArr = randomArray gen split split comp sz
    return $ computeAs B genArr

-- createRandomArray :: (MonadSplit g m, RandomGen g, Index ix) => (ix -> g -> e) -> Comp -> Sz ix -> m (Array D ix e)
-- createRandomArray f comp sz = do
--     genArr <- createGenArray comp sz
--     return $ imap f genArr

sequenceRand :: (Source r ix (m e), MonadSplit g m, MonadRunner g m, RandomGen g) => Comp -> Array r ix (m e) -> m (Array D ix e)
sequenceRand comp arr = do
    genArr <- createGenArray comp (size arr)
    return $ A.zipWith (\a g -> fst $ run a g) arr genArr

getWorkerStates :: (MonadIO m, RandomGen g) => g -> Comp -> m (WorkerStates g)
getWorkerStates gen comp = initWorkerStates comp (\worker -> return $ workerGens !! getWorkerId worker)
    where
        workerGens = gen : map (snd . split) workerGens
