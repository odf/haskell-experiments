-- import some stuff

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!))
import Data.Set (Set, member)


-- a data type for directed graphs (incomplete)

type AdjMap a = Map a [a]

data Graph a = Graph (Set a) (AdjMap a) (AdjMap a) deriving (Eq, Show)

vertex :: (Ord a) => a -> (Graph a) -> Bool
vertex v (Graph verts _ _) = member v verts

source :: (Ord a) => a -> (Graph a) -> Bool
source v g@(Graph _ back _) = (vertex v g) && (null $ back ! v)

sink :: (Ord a) => a -> (Graph a) -> Bool
sink v g@(Graph _ _ forw) = (vertex v g) && (null $ forw ! v)

internal :: (Ord a) => a -> (Graph a) -> Bool
internal v g = (vertex v g) && (not $ source v g) && (not $ sink v g)

isolated :: (Ord a) => a -> (Graph a) -> Bool
isolated v g = (vertex v g) && (source v g) && (sink v g)

edge :: (Ord a) => a -> a -> (Graph a) -> Bool
edge v w g@(Graph _ _ forw) = (vertex v g) && (elem w $ forw ! v)

succs :: (Ord a) => a -> (Graph a) -> [a]
succs v (Graph _ _ forw) = forw ! v

preds :: (Ord a) => a -> (Graph a) -> [a]
preds v (Graph _ back _) = back ! v

adjs :: (Ord a) => a -> (Graph a) -> [a]
adjs v g = concat [succs v g, preds v g]

edges :: (Ord a) => (Graph a) -> [(a, a)]
edges (Graph verts _ forw) = concat $ map edgesFrom $ Set.toList verts
  where edgesFrom = \v -> zip (repeat v) (forw ! v)


-- playing with type class instantiation so I can make lists do arithmetic

instance (Num a) => Num [a] where
  (+)         = zipWith (+)
  (-)         = zipWith (-)
  (*)         = zipWith (*)
  abs         = map abs
  signum      = map signum
  negate      = map negate
  fromInteger = (:[]) . fromInteger

instance (Fractional a) => Fractional [a] where
  (/)          = zipWith (/)
  recip        = map recip
  fromRational = (:[]) . fromRational
