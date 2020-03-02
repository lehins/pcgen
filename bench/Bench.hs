{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

-- base

-- random
import System.Random
-- deepseq
import Control.DeepSeq
-- criterion
import Criterion.Main
-- pcgen
import Data.PCGen

-- | counter -> gen -> result list.
runMany :: RandomGen g => Word -> g -> [Int]
runMany 0 _ = []
runMany w g = let
    (i,g') = next g
    in i : runMany (w-1) g'

-- Our benchmark harness.
main = defaultMain [
    bgroup "runMany 1,000,000" [
        bench "StdGen" $ nf (runMany 1000000) (mkStdGen 501),
        bench "PCGen" $ nf (runMany 1000000) (mkPCGen 501)
        ]
    ]
