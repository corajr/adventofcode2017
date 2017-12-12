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

spec :: Spec
spec = do
  describe "part1" $ do
    let ex' = ex part1
    ex' "ne,ne,ne" 3
    ex' "ne,ne,sw,sw" 0
    ex' "ne,ne,s,s" 2
    ex' "se,sw,se,sw,sw" 3
