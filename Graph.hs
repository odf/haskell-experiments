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


class Eq a => Reticular a ra | ra -> a where
  vertices :: ra -> [GraphItem a]
  edges    :: ra -> [GraphItem a]
  items    :: ra -> [GraphItem a]
  succs    :: ra -> GraphItem a -> [GraphItem a]
  preds    :: ra -> GraphItem a -> [GraphItem a]
  adjs     :: ra -> GraphItem a -> [GraphItem a]
  hasItem  :: ra -> GraphItem a -> Bool
  insert   :: ra -> GraphItem a -> ra
  delete   :: ra -> GraphItem a -> ra

  edges graph = [edge v w | v <- vertices graph, w <- succs graph v]
    where edge v w = Edge (label v) (label w)

  items graph = (vertices graph) ++ (edges graph)

  succs graph (Vertex v) = [Vertex $ to e | e <- edges graph, from e == v]
  succs graph (Edge v w) = [Edge w $ label u | u <- succs graph (Vertex w)]

  preds graph (Vertex w) = [Vertex $ from e | e <- edges graph, to e == w]
  preds graph (Edge v w) = [Edge (label u) v | u <- preds graph (Vertex v)]

  adjs graph item = (preds graph item) ++ (succs graph item)

  hasItem graph = (`elem` items graph)

source :: Reticular a ra => ra -> GraphItem a -> Bool
source graph item@(Vertex _) =
  (hasItem graph item) && (null $ preds graph item)
source _ _ = False

sink :: Reticular a ra => ra -> GraphItem a -> Bool
sink graph item@(Vertex _) =
  (hasItem graph item) && (null $ succs graph item)
sink _ _ = False

internal :: Reticular a ra => ra -> GraphItem a -> Bool
internal g v = (hasItem g v) && (not $ source g v) && (not $ sink g v)

isolated :: Reticular a ra => ra -> GraphItem a -> Bool
isolated g v = (hasItem g v) && (source g v) && (sink g v)

instance (Show a, Reticular a ra) => Show ra where
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
  graph +/ graph' = graph +/ vertices graph' +/ edges graph'
  graph -/ graph' = graph -/ edges graph'


data Graph a = Graph (Set a) (Map a [a]) (Map a [a]) deriving (Eq)

graph :: Ord a => [GraphItem a] -> Graph a
graph = (Graph Set.empty Map.empty Map.empty +/)


instance (Ord a) => Reticular a (Graph a) where
  vertices (Graph verts _ _)        = map Vertex $ Set.toList verts
  
  succs (Graph _ _ forw) (Vertex v) = map Vertex $ forw ! v
  succs graph (Edge v w) = [Edge w $ label u | u <- succs graph (Vertex w)]
  
  preds (Graph _ back _) (Vertex v) = map Vertex $ back ! v
  preds graph (Edge v w) = [Edge (label u) v | u <- preds graph (Vertex v)]

  insert g@(Graph verts back forw) item@(Vertex v)
    | hasItem g item = g
    | otherwise = Graph verts' back' forw'
        where verts' = Set.insert v verts
              back'  = Map.insert v [] back
              forw'  = Map.insert v [] forw
  insert g item@(Edge v w)
    | hasItem g item = g
    | otherwise = Graph verts' back' forw'
        where (Graph verts back forw) = g +/ map Vertex [v, w]
              verts' = verts
              back'  = Map.insertWith' (flip (++)) w [v] back
              forw'  = Map.insertWith' (flip (++)) v [w] forw

  delete g@(Graph verts back forw) item@(Vertex v)
    | not $ hasItem g item = g
    | otherwise            = Graph verts' back' forw'
        where verts'    = Set.delete v verts
              back'     = Map.delete v $ without v back
              forw'     = Map.delete v $ without v forw
              without v = fmap (filter (/= v))
  delete g@(Graph verts back forw) item@(Edge v w)
    | not $ hasItem g item = g
    | otherwise            = Graph verts' back' forw'
        where verts' = verts
              back'  = Map.insertWith (const . filter (/= v)) w [] back
              forw'  = Map.insertWith (const . filter (/= w)) v [] back

  hasItem (Graph verts _ _) (Vertex v) =
    Set.member v verts
  hasItem g@(Graph verts _ forw) (Edge v w) =
    Set.member v verts && (elem w $ forw ! v)

instance (Ord a, Show a) => Show (Graph a) where
  show g = "graph " ++ show ((sort $ edges g) ++ sort isolatedVertices)
    where isolatedVertices = filter (isolated g) $ vertices g
