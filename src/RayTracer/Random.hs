module RayTracer.Random
    ( Gen
    , RandM
    , Seed
    , createGen
    , module System.Random
    , MonadRandom (..)
    , RandT, runRandT, evalRandT, execRandT
    ) where

import System.Random
import Control.Monad.Random.Lazy (RandT, liftRandT, runRandT, evalRandT, execRandT)
import Data.Word (Word32)
import qualified System.Random.MWC as MWC
import Data.Vector (singleton)
import GHC.Base (RealWorld)

type Seed = Word32
newtype Gen = Gen (MWC.Gen RealWorld)
type RandM = RandT Gen IO

createGen :: Seed -> IO Gen
createGen seed = Gen <$> MWC.initialize (singleton seed)

class (Monad m) => MonadRandom m where
  getRandomR :: MWC.Variate a => (a, a) -> m a
  getRandom :: MWC.Variate a => m a

instance MonadRandom RandM where
  getRandomR range = liftRandT $ \(Gen gen) -> do
    result <- MWC.uniformR range gen
    return (result, Gen gen)
  getRandom = liftRandT $ \(Gen gen) -> do
    result <- MWC.uniform gen
    return (result, Gen gen)
