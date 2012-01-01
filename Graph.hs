{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, OverlappingInstances #-}

-- a data type for directed graphs (incomplete)

module Graph where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!))
import Data.Set (Set)
import Data.List (sort)


data GraphItem a = Vertex { label :: a }
                 | Edge { from :: a, to :: a } deriving (Ord, Eq)

instance Show a => Show (GraphItem a) where
  show (Vertex v) = "Vertex " ++ show v
  show (Edge v w) = "Edge " ++ show v ++ " " ++ show w


class Reticular a ra | ra -> a where
  empty    :: ra
  vertices :: ra -> [GraphItem a]
  succs    :: ra -> GraphItem a -> [GraphItem a]
  preds    :: ra -> GraphItem a -> [GraphItem a]
  insert   :: ra -> GraphItem a -> ra
  delete   :: ra -> GraphItem a -> ra

  contains   :: (Eq a) => ra -> GraphItem a -> Bool
  contains g item@(Vertex _) = item `elem` vertices g
  contains g item@(Edge _ _) = item `elem` edges g

  adjs     :: ra -> GraphItem a -> [GraphItem a]
  adjs graph item = (preds graph item) ++ (succs graph item)

  edges    :: ra -> [GraphItem a]
  edges graph = concat $ map outOf $ vertices graph
    where outOf item   = zipWith makeEdge (repeat item) (succs graph item)
          makeEdge v w = Edge (label v) (label w)

  graph :: [GraphItem a] -> ra
  graph = (empty +/)

source :: (Eq a, Reticular a ra) => ra -> GraphItem a -> Bool
source graph item@(Vertex _) =
  (contains graph item) && (null $ preds graph item)
source _ _ = False

sink :: (Eq a, Reticular a ra) => ra -> GraphItem a -> Bool
sink graph item@(Vertex _) =
  (contains graph item) && (null $ succs graph item)
sink _ _ = False

internal :: (Eq a, Reticular a ra) => ra -> GraphItem a -> Bool
internal g v = (contains g v) && (not $ source g v) && (not $ sink g v)

isolated :: (Eq a, Reticular a ra) => ra -> GraphItem a -> Bool
isolated g v = (contains g v) && (source g v) && (sink g v)

instance (Eq a, Show a, Reticular a ra) => Show ra where
  show g = "graph " ++ show ((edges g) ++ isolatedVertices)
    where isolatedVertices = filter (isolated g) $ vertices g


class RPart a pa | pa -> a where
  (+/) :: (RPart a pa, Reticular a ra) => ra -> pa -> ra
  (-/) :: (RPart a pa, Reticular a ra) => ra -> pa -> ra

instance RPart a (GraphItem a) where
  (+/) = insert
  (-/) = delete

instance RPart a [GraphItem a] where
  (+/) = foldl insert
  (-/) = foldl delete

instance (Reticular a ra) => RPart a ra where
  graph +/ graph' = graph +/ (vertices graph') +/ (edges graph')
  graph -/ graph' = graph -/ (edges graph')


data Graph a = Graph (Set a) (Map a [a]) (Map a [a]) deriving (Eq)

instance (Ord a) => Reticular a (Graph a) where
  empty                             = Graph Set.empty Map.empty Map.empty
  vertices (Graph verts _ _)        = map Vertex $ Set.toList verts
  succs (Graph _ _ forw) (Vertex v) = map Vertex $ forw ! v
  preds (Graph _ back _) (Vertex v) = map Vertex $ back ! v

  insert g item
    | contains g item = g
    | otherwise       = insert' item g

  delete g item
    | not $ contains g item = g
    | otherwise             = delete' item g

  contains (Graph verts _ _) (Vertex v) =
    Set.member v verts
  contains g@(Graph _ _ forw) (Edge v w) =
    (contains g (Vertex v)) && (elem w $ forw ! v)


insert' :: (Ord a) => GraphItem a -> Graph a -> Graph a
insert' (Vertex v) (Graph verts back forw) =
  let verts' = Set.insert v verts
      back'  = Map.insert v [] back
      forw'  = Map.insert v [] forw
  in Graph verts' back' forw'
insert' (Edge v w) g =
  let (Graph verts back forw) = foldl insert g $ map Vertex [v, w]
      verts' = verts
      back'  = Map.insert w (back ! w ++ [v]) back
      forw'  = Map.insert v (forw ! v ++ [w]) forw
  in Graph verts' back' forw'

delete' :: (Ord a) => GraphItem a -> Graph a -> Graph a
delete' (Vertex v) (Graph verts back forw) =
  let verts'    = Set.delete v verts
      back'     = Map.delete v $ without v back
      forw'     = Map.delete v $ without v forw
      without v = fmap (filter (/= v))
  in Graph verts' back' forw'
delete' (Edge v w) (Graph verts back forw) =
  let verts' = verts
      back'  = Map.insert w (filter (/= v) $ back ! w) back
      forw'  = Map.insert v (filter (/= w) $ forw ! v) forw
  in Graph verts' back' forw' 

instance (Ord a, Show a) => Show (Graph a) where
  show g = "graph " ++ show ((sort $ edges g) ++ sort isolatedVertices)
    where isolatedVertices = filter (isolated g) $ vertices g
