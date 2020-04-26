{-# LANGUAGE FlexibleContexts #-}

module RayTracer.Random 
    ( Gen
    , RandM
    , createGen
    , createGenArray
    , sequenceRand
    , module System.Random
    , MonadRandom (..)
    , module Control.Monad.Random.Lazy
    ) where

import System.Random
import Control.Monad.Random hiding (uniform)
import Control.Monad.Random.Lazy hiding (uniform)
import System.Random.SplitMix
import Data.Massiv.Array hiding (map)
import qualified Data.Massiv.Array as A
import Data.Word (Word64)

type Gen = SMGen
type RandM = Rand Gen

createGen :: Word64 -> Gen
createGen = mkSMGen

createGenArray :: (MonadSplit g m, RandomGen g, Index ix) => Comp -> Sz ix -> m (Array B ix g)
createGenArray comp sz = do
    gen <- getSplit
    let genArr = randomArray gen split split comp sz
    return $ computeAs B genArr

sequenceRand :: (Source r ix (Rand g e), RandomGen g) => Comp -> Array r ix (Rand g e) -> Rand g (Array D ix e)
sequenceRand comp arr = do
    genArr <- createGenArray comp (size arr)
    return $ A.zipWith (\a g -> fst $ runRand a g) arr genArr
