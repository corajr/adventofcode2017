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

particle1 = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
particle2 = "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"

particle1parsed = Particle (3,0,0) (2,0,0) (-1,0,0)
particle2parsed = Particle (4,0,0) (0,0,0) (-2,0,0)

particles = unlines [particle1, particle2]

spec :: Spec
spec = do
  describe "pParticle" $ do
    let ex' = ex (parse pParticle "")
    ex' particle1 (Right particle1parsed)
    ex' particle2 (Right particle2parsed)
  describe "part1" $ do
    let ex' = ex part1
    ex' particles 0
