{-# LANGUAGE FlexibleContexts #-}

module RayTracer.Random 
    ( Gen
    , createGen
    , createGenArray
    , getWorkerStates
    , createRandomArray
    , module System.Random
    ) where

import System.Random
import System.Random.SplitMix
import Data.Massiv.Array as A hiding (map)
import Data.Word (Word64)

import Data.Massiv.Core
import Control.Scheduler


type Gen = SMGen

createGen :: Word64 -> Gen
createGen = mkSMGen

getWorkerStates :: (RandomGen g) => g -> Comp -> IO (WorkerStates g)
getWorkerStates gen comp = initWorkerStates comp (\worker -> return $ workerGens !! getWorkerId worker)
    where
        workerGens = gen : map (snd . split) workerGens

createGenArray :: (RandomGen g, Index ix) => g -> Comp -> Sz ix -> Array B ix g
createGenArray gen comp sz = computeAs B $ randomArray gen split split comp sz

createRandomArray :: (RandomGen g, Index ix) => g -> (ix -> g -> e) -> Comp -> Sz ix -> Array D ix e
createRandomArray gen f comp sz = imap f $ createGenArray gen comp sz
