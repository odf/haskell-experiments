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
  succs    :: GraphItem a -> ra -> [GraphItem a]
  preds    :: GraphItem a -> ra -> [GraphItem a]
  insert   :: GraphItem a -> ra -> ra
  delete   :: GraphItem a -> ra -> ra

  member   :: (Eq a) => GraphItem a -> ra -> Bool
  member item@(Vertex _) = (item `elem`) . vertices
  member item@(Edge _ _) = (item `elem`) . edges

  adjs     :: GraphItem a -> ra -> [GraphItem a]
  adjs item graph = (preds item graph) ++ (succs item graph)

  edges    :: ra -> [GraphItem a]
  edges graph = concat $ map outOf $ vertices graph
    where outOf item   = zipWith makeEdge (repeat item) (succs item graph)
          makeEdge v w = Edge (label v) (label w)

  insertAll :: [GraphItem a] -> ra -> ra
  insertAll = flip $ foldr insert

  deleteAll :: [GraphItem a] -> ra -> ra
  deleteAll = flip $ foldr delete

  graph :: [GraphItem a] -> ra
  graph = (`insertAll` empty)


source :: (Eq a, Reticular a ra) => GraphItem a -> ra -> Bool
source item@(Vertex _) graph =
  (member item graph) && (null $ preds item graph)
source _ _ = False

sink :: (Eq a, Reticular a ra) => GraphItem a -> ra -> Bool
sink item@(Vertex _) graph =
  (member item graph) && (null $ succs item graph)
sink _ _ = False

internal :: (Eq a, Reticular a ra) => GraphItem a -> ra -> Bool
internal v g = (member v g) && (not $ source v g) && (not $ sink v g)

isolated :: (Eq a, Reticular a ra) => GraphItem a -> ra -> Bool
isolated v g = (member v g) && (source v g) && (sink v g)

instance (Eq a, Show a, Reticular a ra) => Show ra where
  show g = "graph " ++ show ((edges g) ++ isolatedVertices)
    where isolatedVertices = filter (`isolated` g) $ vertices g


data Graph a = Graph (Set a) (Map a [a]) (Map a [a]) deriving (Eq)

instance (Ord a) => Reticular a (Graph a) where
  empty                             = Graph Set.empty Map.empty Map.empty
  vertices (Graph verts _ _)        = map Vertex $ Set.toList verts
  succs (Vertex v) (Graph _ _ forw) = map Vertex $ forw ! v
  preds (Vertex v) (Graph _ back _) = map Vertex $ back ! v

  insert item g
    | member item g = g
    | otherwise     = insert' item g

  delete item g
    | not $ member item g = g
    | otherwise           = delete' item g

  member (Vertex v) (Graph verts _ _)  =
    Set.member v verts
  member (Edge v w) g@(Graph _ _ forw) =
    (member (Vertex v) g) && (elem w $ forw ! v)


insert' :: (Ord a) => GraphItem a -> Graph a -> Graph a
insert' (Vertex v) (Graph verts back forw) =
  let verts' = Set.insert v verts
      back'  = Map.insert v [] back
      forw'  = Map.insert v [] forw
  in Graph verts' back' forw'
insert' (Edge v w) g =
  let (Graph verts back forw) = foldr insert g $ map Vertex [v, w]
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
    where isolatedVertices = filter (`isolated` g) $ vertices g
