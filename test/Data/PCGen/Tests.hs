{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PCGen.Tests (
    pcgenTests
    ) where

-- base
import Data.Word
-- hspec
import Test.Hspec
import Test.QuickCheck
-- random
import System.Random
-- pcgen
import Data.PCGen

-- | This is an Orphan Instance, but that's totally alright pal, because we're
-- only using this during a testing suite.
instance Arbitrary PCGen where
    arbitrary = do
        w1 <- arbitrary
        return (mkPCGen (w1 :: Word))

propGenRange :: (RandomGen g) => g -> g -> Bool
propGenRange = \rgenA rgenB -> genRange rgenA == genRange rgenB

propNextInRange :: (RandomGen g) => g -> Bool
propNextInRange = \randomGen -> let
        (i,g) = next randomGen
        (low, high) = genRange g
        in i >= low && i <= high

propSplit :: (RandomGen g, Eq g) => g -> Bool
propSplit = \randomGen -> let
    (genL, genR) = split randomGen
    in (genL /= randomGen) && (genR /= randomGen) && (genL /= genR)

oddInc :: PCGen -> Bool
oddInc gen = odd $ (read $ last $ words $ show $ gen :: Word64)

pcgenTests :: Spec
pcgenTests = describe "PCGen" $ parallel $ do
    it "gives the same output as the C reference impl" $
        let p = PCGen 505 505
            oneStep = stepGen p
            twoSteps = stepGen $ snd oneStep
            threeSteps = stepGen $ snd twoSteps
        in
            fst oneStep == 0 && snd oneStep == PCGen 4155324217168486846 505 &&
            fst twoSteps == 2926225613 && snd twoSteps == PCGen 2179395809720005215 505 &&
            fst threeSteps == 1492848122 && snd threeSteps == PCGen 15704844188202024364 505

    it "genRange ignores the input generator given" $ property $
        \genA genB -> propGenRange (genA::PCGen) (genB::PCGen)
        
    it "next is always within the bounds given by genRange" $ property $
        \gen -> propNextInRange (gen::PCGen)
    
    it "next doesn't change inc" $ property $
        \gen -> let (_,g') = next (gen::PCGen) in case (gen, g') of
            (PCGen _ incBefore, PCGen _ incAfter) -> incBefore == incAfter

    it "split results are distinct from both the input and each other" $ property $
        \gen -> propSplit (gen::PCGen)
        
    it "split results both have an odd inc value" $ property $
        \gen -> let (a,b) = split (gen::PCGen) in oddInc a && oddInc b

    it "read.show == id" $ property $
        \gen -> gen == (read (show (gen::PCGen)))
