module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import qualified Data.Vector.Unboxed as V

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

exampleFirewall = unlines
  [ "0: 3"
  , "1: 2"
  , "4: 4"
  , "6: 4"
  ]

spec :: Spec
spec = do
  describe "parseFirewall" $ do
    let ex' = ex parseFirewall
    ex' exampleFirewall (V.fromList [3, 2, minBound, minBound, 4, minBound, 4])
  describe "part1" $ do
    let ex' = ex part1
    ex' exampleFirewall 24
  describe "part2" $ do
    let ex' = ex part2
    ex' exampleFirewall 10
