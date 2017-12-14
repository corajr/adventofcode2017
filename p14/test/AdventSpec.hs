module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as Set

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
    ex' "flqrgnkx" 8108
  describe "makeSetOfBoolMatrix" $ do
    let ex' = ex makeSetOfBoolMatrix
    ex' [] Set.empty
    ex' [[True]] (Set.singleton (0, 0))
    ex' [[True, False, True]] (Set.fromList [(0, 0), (0, 2)])
    ex' [[True, False, True], [False, True]] (Set.fromList [(0, 0), (0, 2), (1, 1)])
  describe "part2" $ do
    let ex' = ex part2
    ex' "flqrgnkx" 1242
