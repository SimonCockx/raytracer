module RayTracer.Random
    ( Gen
    , RandM
    , Seed
    , createGen
--    , createGenArray
--    , sequenceRand
    , module System.Random
    , MonadRandom (..)
    , module Control.Monad.Random.Lazy
    ) where

import System.Random
import Control.Monad.Random hiding (uniform)
import Control.Monad.Random.Lazy hiding (uniform)
import System.Random.SplitMix
import qualified System.Random.MWC as MWC
-- import Data.Massiv.Array hiding (map, singleton)
-- import qualified Data.Massiv.Array as A
import Data.Word (Word64)
import Data.Vector (singleton)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Random.Mersenne.Pure64 (PureMT, pureMT)

type Seed = Word64
type Gen = SMGen
type RandM = Rand Gen

createGen :: Seed -> IO Gen
createGen = return . mkSMGen



--newtype MWCGen = MWVGen MWC.GenIO
--
--instance RandomGen MWCGen where
--  next (MWCGen gen) = (unsafePerformIO $ MWC.uniform gen, MWCGen gen)
--  split _ = error "This random generator cannot be split"


--createGenArray :: (MonadSplit g m, RandomGen g, Index ix) => Comp -> Sz ix -> m (Array B ix g)
--createGenArray comp sz = do
--    gen <- getSplit
--    let genArr = randomArray gen split split comp sz
--    return $ computeAs B genArr
--
--sequenceRand :: (Source r ix (Rand g e), RandomGen g) => Comp -> Array r ix (Rand g e) -> Rand g (Array D ix e)
--sequenceRand comp arr = do
--    genArr <- createGenArray comp (size arr)
--    return $ A.zipWith (\a g -> fst $ runRand a g) arr genArr
