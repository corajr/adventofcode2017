module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Control.Monad (forM_, replicateM)
import qualified Data.Vector as V
-- import Control.Monad.State.Strict (execState, evalState)

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

exampleMap = unlines
  [ "..#"
  , "#.."
  , "..."
  ]

spec :: Spec
spec = do
  describe "part1" $ do
    let ex' = ex . part1
    ex' 7 exampleMap 5
    ex' 70 exampleMap 41
    ex' 10000 exampleMap 5587
  describe "part2" $ do
    let ex' = ex . part2
    ex' 100 exampleMap 26
