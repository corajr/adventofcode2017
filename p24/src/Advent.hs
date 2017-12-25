module Advent
 ( Pins(..)
 , Port(..)
 , mkPort
 , pPorts
 , canConnect
 , connect
 , remove
 , doConnect
 , getPossibleNext
 , bridgesFrom
 , starters
 , portMaps
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

type PortMap = Map Int (Seq Port)
type PortMaps = (PortMap, PortMap)

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

portMap :: (Port -> Pins) -> [Port] -> PortMap
portMap f = Map.fromListWith (Seq.><) . concatMap g
  where
    g x = case f x of
      Closed _ -> []
      Open x' -> [(x', Seq.singleton x)]

deleteAt :: Int -> Seq a -> Seq a
deleteAt i xs =
  let (ys, zs) = Seq.splitAt i xs
  in ys Seq.>< Seq.drop 1 zs

modifyFirstInstanceOf :: (Eq a) => (Int -> Seq a -> Seq a) -> a -> Seq a -> Seq a
modifyFirstInstanceOf f x xs = case Seq.elemIndexL x xs of
  Just i -> f i xs
  Nothing -> xs

clearEmpties :: (Eq a) => (a -> Seq a -> Seq a) -> a -> Seq a -> Maybe (Seq a)
clearEmpties f x xs
  | Seq.null xs' = Nothing
  | otherwise = Just xs'
  where xs' = f x xs

deleteFirst :: (Eq a) => a -> Seq a -> Seq a
deleteFirst x = modifyFirstInstanceOf deleteAt x

deleteFirst' = clearEmpties deleteFirst

doUpdate :: (Port -> Seq Port -> Maybe (Seq Port)) -> PortMaps -> Port -> PortMaps
doUpdate f (firstMap, secondMap) x@(Port a b) =
  let firstMap' = Map.update (f x) (unPins a) firstMap
      secondMap' = Map.update (f x) (unPins b) secondMap
  in (firstMap', secondMap')

portMaps :: [Port] -> PortMaps
portMaps = portMap side1 &&& portMap side2

remove :: PortMaps -> Port -> PortMaps
remove = doUpdate deleteFirst'

addOrAdjust :: (Seq Port -> Seq Port) -> Int -> PortMap -> PortMap
addOrAdjust f i = Map.alter f' i
  where
    f' Nothing = Just (f Seq.empty)
    f' (Just xs) = Just (f xs)

insert (Port (Closed _) (Closed _)) maps = maps
insert x@(Port (Open a) (Closed _)) (firstMap, secondMap) =
  (addOrAdjust (Seq.|> x) a firstMap, secondMap)
insert x@(Port (Closed _) (Open b)) (firstMap, secondMap) =
  (firstMap, addOrAdjust (Seq.|> x) b secondMap)
insert x@(Port (Open a) (Open b)) (firstMap, secondMap) =
  (addOrAdjust (Seq.|> x) a firstMap, addOrAdjust (Seq.|> x) b secondMap)

doConnect :: Port -> Port -> PortMaps -> [(Port, PortMaps)]
doConnect start end maps = do
  (start', end') <- connect start end
  let maps' = insert start' $ remove maps start
      maps'' = insert end' $ remove maps' end
  pure $ (end, maps'')

getPossibleNext :: Port -> PortMaps -> [(Port, PortMaps)]
getPossibleNext port maps =
  let (firstMap, secondMap) = maps `remove` port
  in concatMap (\x -> doConnect port x maps) $ do
    pins <- openPins port
    let xs = toList $ Map.findWithDefault Seq.empty (unPins pins) firstMap
        (_, secondMap') = foldl' remove (firstMap, secondMap) xs
        ys = toList $ Map.findWithDefault Seq.empty (unPins pins) secondMap'
    xs ++ ys

starters :: PortMaps -> [(Port, PortMaps)]
starters = getPossibleNext (Port (Closed 0) (Open 0))

score :: [Port] -> Int
score [] = 0
score (x : xs) = sumPins x + score xs

bridgesFrom :: [(Port, PortMaps)] -> [[Port]]
bridgesFrom [] = [[]]
bridgesFrom ((x, maps) : xs) = nexts' ++ bridgesFrom xs
  where
    nexts = getPossibleNext x maps
    nexts' = map (x:) $ bridgesFrom nexts

maxBridge :: PortMaps -> Int
maxBridge maps = maximum . map score $ bridgesFrom (starters maps)

part1 :: String -> Int
part1 = either (error . show) (maxBridge . portMaps) . parse pPorts ""

part2 = undefined

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/24.txt"
  print $ part1 input
