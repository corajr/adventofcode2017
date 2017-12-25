module Advent
 ( Pins(..)
 , Port(..)
 , mkPort
 , pPorts
 , canConnect
 , connect
 , insert
 , remove
 , doConnect
 , getPossibleNext
 , bridgesFrom
 , starters
 , portMap
 , part1
 , part2
 , cliMain
 ) where


import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.Function (on)
import Data.Sequence (Seq)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Debug.Trace (traceShowId)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

data Pins = Closed { unPins :: Int }
          | Open { unPins :: Int }
  deriving (Eq, Ord)

instance Show Pins where
  show (Closed x) = "X" ++ show x
  show (Open x) = "O" ++ show x

data Port = Port { side1 :: Pins, side2 :: Pins}
  deriving (Eq, Ord)

instance Show Port where
  show (Port a b) = show a ++ "-" ++ show b

type Count = Int
type Ports = Map Port Count

type PortMap = Map Int Ports

mkPort :: Int -> Int -> Port
mkPort a b = if a < b then Port (Open a) (Open b) else Port (Open b) (Open a)

pPort = mkPort <$> int <*> (char '/' *> int)
pPorts = pPort `endBy` newline

openPins :: Port -> [Pins]
openPins (Port a b) = f a ++ f b
  where
    f x@(Open _) = [x]
    f (Closed _) = []

openPinsAndClosedVersion :: Port -> [(Pins, Port)]
openPinsAndClosedVersion (Port (Closed _) (Closed _)) = []
openPinsAndClosedVersion (Port a@(Open a') b@(Closed _)) = [(a, Port (Closed a') b)]
openPinsAndClosedVersion (Port a@(Closed _) b@(Open b')) = [(b, Port a (Closed b'))]
openPinsAndClosedVersion (Port a@(Open a') b@(Open b')) =
  [ (a, Port (Closed a') b)
  , (b, Port a (Closed b'))
  ]

sumPins :: Port -> Int
sumPins (Port a b) = unPins a + unPins b

canConnect x y = or $ do
  one <- openPins x
  two <- openPins y
  pure $ one == two

connect :: Port -> Port -> [(Port, Port)]
connect x y = do
  (a, x') <- openPinsAndClosedVersion x
  (b, y') <- openPinsAndClosedVersion y
  if a == b then pure (x', y') else []

portMap :: [Port] -> PortMap
portMap = foldl' (flip insert) Map.empty

innerInsert :: Port -> Ports -> Ports
innerInsert x = Map.alter g x
  where
    g (Just count) = Just (count + 1)
    g Nothing = Just 1

nonemptyMap :: Map k a -> Maybe (Map k a)
nonemptyMap m = if Map.null m then Nothing else Just m

insert :: Port -> PortMap -> PortMap
insert (Port (Closed _) (Closed _)) m = m
insert port@(Port a b) m = m''
  where
    m' = case a of
      Closed _ -> m
      Open a' -> Map.alter g a' m
    m'' = case b of
      Closed _ -> m'
      Open b' -> Map.alter g b' m'
    g (Just inner) = nonemptyMap $ innerInsert port inner
    g Nothing = nonemptyMap $ innerInsert port Map.empty

innerRemove :: Port -> Ports -> Ports
innerRemove x = Map.alter g x
  where
    g (Just count) = if count == 1 then Nothing else Just (count - 1)
    g Nothing = Nothing

remove :: Port -> PortMap -> PortMap
remove port@(Port a b) m =
  Map.alter g a' . Map.alter g b' $ m
  where
    a' = unPins a
    b' = unPins b
    g (Just inner) = nonemptyMap $ innerRemove port inner
    g Nothing = Nothing

doConnect :: Port -> Port -> PortMap -> [(Port, PortMap)]
doConnect start end availablePorts = do
  (start', end') <- connect start end
  let availablePorts' = insert start' $ remove start availablePorts
      availablePorts'' = insert end' $ remove end availablePorts'
  pure $ (end', availablePorts'')

getPossibleNext :: Port -> PortMap -> [(Port, PortMap)]
getPossibleNext port availablePorts =
  let availablePorts' = remove port availablePorts
  in concatMap (\x -> doConnect port x availablePorts') $ do
    pins <- openPins port
    Map.keys $ Map.findWithDefault Map.empty (unPins pins) availablePorts'

starters :: PortMap -> [(Port, PortMap)]
starters = getPossibleNext (Port (Closed 0) (Open 0))

score :: [Port] -> Int
score [] = 0
score (x : xs) = sumPins x + score xs

score2 :: [Port] -> (Int, Int)
score2 [] = (0, 0)
score2 (x : xs) = let (a, b) = score2 xs
                  in (a + 1, b + sumPins x)

bridgesFrom :: [(Port, PortMap)] -> [[Port]]
bridgesFrom [] = [[]]
bridgesFrom ((x, availablePorts) : xs) = nexts' ++ bridgesFrom xs
  where
    nexts = getPossibleNext x availablePorts
    nexts' = map (x:) $ bridgesFrom nexts

maxBridge :: PortMap -> Int
maxBridge availablePorts = maximum . map score $ bridgesFrom (starters availablePorts)

maxLengthBridge :: PortMap -> Int
maxLengthBridge availablePorts = snd . maximum . map score2 $ bridgesFrom (starters availablePorts)

part1 :: String -> Int
part1 = either (error . show) (maxBridge . portMap) . parse pPorts ""

part2 = either (error . show) (maxLengthBridge . portMap) . parse pPorts ""

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/24.txt"
  print $ part2 input
