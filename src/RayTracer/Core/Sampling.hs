module Sampling
    ( SamplingStrategy (..)
    ) where


data SamplingStrategy 
    = RegularGrid Int
    | Random Int
    | Jittered Int
    deriving (Show)