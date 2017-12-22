module Advent where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Function (on)
import Data.List (splitAt)
import Debug.Trace (traceShowId)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

type Triple = (Int, Int, Int)

data Particle = Particle
  { position :: Triple
  , velocity :: Triple
  , acceleration :: Triple
  } deriving (Eq, Show)

instance Ord Particle where
  compare = compare `on` manhattan

manhattan (Particle (a, b, c) _ _) = sum [abs a, abs b, abs c]

pTuple = do
  char '<'
  [a, b, c] <- int `sepBy` char ','
  char '>'
  pure $ (a, b, c)

pParticle = do
  string "p="
  pos <- pTuple
  string ", v="
  velo <- pTuple
  string ", a="
  accel <- pTuple
  pure $ Particle pos velo accel

pParticles = V.fromList <$> pParticle `endBy` newline

type Particles = V.Vector Particle

step :: Particle -> Particle
step (Particle (x, y, z) (vX, vY, vZ) (aX, aY, aZ)) =
  let (vX', vY', vZ') = (vX + aX, vY + aY, vZ + aZ)
      (x', y', z') = (x + vX', y + vY', z + vZ')
  in Particle (x', y', z') (vX', vY', vZ') (aX, aY, aZ)

converge :: (Eq a) => Int -> [a] -> a
converge n xs = let (x:xs', ys) = splitAt n xs
                in if all (== x) xs' then x else converge n ys


particleMapFromList :: [Particle] -> Map Triple [Particle]
particleMapFromList = Map.fromListWith (++) . map (position &&& (:[]) . id)

collide :: Map Triple [Particle] -> Map Triple [Particle]
collide = Map.filter (\xs -> length xs == 1) . particleMapFromList . map step . concat . Map.elems

part1 :: String -> Int
part1 = either (error . show) (converge 1000 . map fst . iterate f . (\x -> (-1, x))) . parse pParticles ""
  where
    f (_, particles) = let particles' = V.map step particles
                       in (V.minIndex particles', particles')

part2 :: String -> Int
part2 = either (error . show) (converge 100 . map Map.size . iterate collide . particleMapFromList  . V.toList) . parse pParticles ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/20.txt"
  print $ part2 input
