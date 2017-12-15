module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

genA = [1092455, 1181022009, 245556042, 1744312007, 1352636452]
genB = [430625591, 1233683848, 1431495498, 137874439, 285222916]

spec :: Spec
spec = do
  describe "generator" $ do
    let ex' gen = ex (take 5 . gen)
    ex' generatorA 65 genA
    ex' generatorB 8921 genB
  describe "judge" $ do
    it "returns 1 for an example short list" $
      judge (zip genA genB) `shouldBe` 1
  describe "part1" $ do
    let ex' = ex part1
    ex' (65, 8921) 588
  describe "part2" $ do
    let ex' = ex part1
    ex' (65, 8921) 309
