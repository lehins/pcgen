{-# LANGUAGE Safe #-}

-- | This module contains a permuted linear congruential pseudorandom number
-- generator, as described by M.E. O'Neill (<pcg-random.org>). This version
-- holds two 'Word64' values and outputs a 'Word32' of randomness each time you
-- use it. Compared to the 'StdGen' type from "System.Random", it's around a
-- 2.5x to 3x speedup on a 64-bit system. It runs somewhat slower than @StdGen@
-- on a 32-bit system (the @Word64@ values must be emulated after all), but
-- there aren't too many of those in play these days anyway. Raspberry Pi is the
-- only common thing I can think of, which isn't the best for Haskell in the
-- first place. If you somehow are using 32-bit all the time I guess this module
-- isn't for you.
--
-- The first @Word64@ is the @state@, which changes with each step of the
-- generator. The second @Word64@ is the @inc@, which stays fixed from step to
-- step and controls what stream of numbers is generated. The @inc@ is always
-- bitwise OR'd with 1 during computations, so there are only 2^63 distinct
-- number streams (eg: @inc@ 2 and 3 are the same number stream). The @state@
-- value eventually returns to it's initial value after 2^64 uses and the whole
-- sequence loops around.
module Data.PCGen (
    PCGen(PCGen),
    mkPCGen,
    stepGen
    ) where

-- base
import Data.Bits
import Data.Word (Word32,Word64)
import Data.Int (Int32)
import Foreign.Storable
import Foreign.Ptr
import Control.Exception
-- random
import System.Random

-- | The @PCGen@ data type. You can use the constructor yourself with two
-- @Word64@ values, but with human picked seeds the first result will generally
-- be 0. To avoid that you can use the 'mkPCGen' helper.
data PCGen = PCGen !Word64 !Word64
    -- Programmer's Note: GHC unpacks small fields by default when they're
    -- strict, and if the user for some reason *did* turn that feature off, then
    -- we should respect their wishes and not unpack our crap in their space. So
    -- we deliberately leave out the UNPACK directive on these fields.
    deriving (Read, Show, Eq, Ord)

-- | Creates a new PCGen value by using the Integral given as both the @state@
-- and @inc@ values for the generator. The state of the generator is then
-- advanced once, because otherwise the first result tends to be 0 with human
-- picked seeds.
mkPCGen :: Integral i => i -> PCGen
mkPCGen n = let
    n' = fromIntegral n
    in snd $ stepGen $ PCGen n' n'

-- | Advances the given generator one step, giving back a @Word32@ of output and
-- the resultant generator as well. This is the most basic way to advance the
-- generator. You probably want to use the 'RandomGen' instance and the 'next'
-- method, along with something like 'MonadRandom'
stepGen :: PCGen -> (Word32,PCGen)
stepGen (PCGen state inc) = let
    newState = state * 6364136223846793005 + (inc .|. 1)
    xorShifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
    rot = fromIntegral (state `shiftR` 59) :: Word32
    out = (xorShifted `shiftR` (fromIntegral rot)) .|. (xorShifted `shiftL` fromIntegral ((-rot) .&. 31))
    in (out, PCGen newState inc)

instance RandomGen PCGen where
    -- 'next' specifies that the output is an Int value, so we convert the
    -- Word32 bits into the Int32 range before converting that into an Int and
    -- returning. If we convert the Word32 directly to Int and Int is Int64,
    -- then our outputs won't match up properly with what we declare with the
    -- genRange function. Of course, all this number conversion stuff between
    -- Integral types is pretty much noop nonsense once we compile, but Haskell
    -- likes to be very precise with types after all.
    next gen = let
        (outWord, nextGen) = stepGen gen
        outInt = fromIntegral (fromIntegral outWord :: Int32) :: Int
        in (outInt, nextGen)

    -- Similar to the above, the range of a PCGen is 32-bits of output per step,
    -- and so we use the bounds of Int32, but we have to convert that into Int
    -- to conform with the spec of the typeclass.
    genRange _ = (fromIntegral (minBound :: Int32),
                    fromIntegral (maxBound :: Int32))

    -- The only real spec here is that the two result generators be dissimilar
    -- from each other and also from the input generator. So we just do some
    -- nonsense shuffling around to achieve that.
    split gen@(PCGen state inc) = let -- no statistical foundation for this!
        (q,nGen1@(PCGen sa ia)) = stepGen gen
        (w,nGen2@(PCGen sb ib)) = stepGen nGen1
        (e,nGen3@(PCGen sc ic)) = stepGen nGen2
        (r,nGen4@(PCGen sd id)) = stepGen nGen3
        stateA = sd `rotateR` 5
        stateB = sd `rotateR` 3
        incA = ((fromIntegral q) `shiftL` 32) .|. (fromIntegral w)
        incB = ((fromIntegral e) `shiftL` 32) .|. (fromIntegral r)
        outA = PCGen stateA (incA .|. 1)
        outB = PCGen stateB (incB .|. 1)
        in (outA, outB)
        -- TODO: This could probably be faster while still conforming to spec.

instance Random PCGen where
    -- produces a random PCGen value.
    random gen = let
        (x,newGen) = random gen
        in (mkPCGen (x::Word),newGen)
    
    -- produces a random PCGen value but ignores the range input since it would
    -- be nonsensical to try and use them as the "bounds" of anything.
    randomR _ gen = random gen

instance Storable PCGen where
    sizeOf _ = sizeOf (undefined :: Word64) * 2
    alignment _ = alignment (undefined :: Word64)
    peek ptr = do
        if alignPtr ptr (alignment (undefined :: PCGen)) == ptr
            then do
                let word64Ptr = castPtr ptr
                    offset = sizeOf (undefined :: Word64)
                s <- peek word64Ptr :: IO Word64
                i <- peek (plusPtr word64Ptr offset) :: IO Word64
                pure $ PCGen s i
            else error "The Ptr is not correctly aligned"
    poke ptr (PCGen s i) = do
        if alignPtr ptr (alignment (undefined :: PCGen)) == ptr
            then do
                let word64Ptr = castPtr ptr
                    offset = sizeOf (undefined :: Word64)
                poke word64Ptr s
                poke (plusPtr word64Ptr offset) i
            else error "The Ptr is not correctly aligned"
