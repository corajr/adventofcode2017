module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector.Unboxed as V
import Text.Parsec

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

exampleMoves = "s1,x3/4,pe/b"
exampleMovesParsed = [Spin 1, Exchange 3 4, Partner 'e' 'b']

exampleMoveResults = ["eabcd", "eabdc", "baedc"]

spec :: Spec
spec = do
  describe "pMoves" $ do
    let ex' = ex (parse pMoves "")
    ex' exampleMoves (Right exampleMovesParsed)
  describe "executeMove" $ do
    let ex' start = ex (V.toList . executeMove (V.fromList start))
    ex' "abcde" (Spin 1) "eabcd"
    ex' "eabcd" (Exchange 3 4) "eabdc"
    ex' "eabdc" (Partner 'e' 'b') "baedc"
