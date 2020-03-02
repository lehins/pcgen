
module Main where

-- hspec
import Test.Hspec
-- pcgen
import Data.PCGen.Tests

main :: IO ()
main = hspec $ do
    pcgenTests
