{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

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

  edges graph = [edge v w | v <- vertices graph, w <- succs graph v]
    where edge v w = Edge (label v) (label w)

  items graph = (vertices graph) ++ (edges graph)

  succs graph (Vertex v) = [Vertex $ to e | e <- edges graph, from e == v]
  succs graph (Edge v w) = [Edge w $ label u | u <- succs graph (Vertex w)]

  preds graph (Vertex w) = [Vertex $ from e | e <- edges graph, to e == w]
  preds graph (Edge v w) = [Edge (label u) v | u <- preds graph (Vertex v)]

  adjs graph item = (preds graph item) ++ (succs graph item)

  hasItem graph = (`elem` items graph)

hasSource :: Reticular a ra => ra -> GraphItem a -> Bool
hasSource graph item@(Vertex _) =
  (hasItem graph item) && (null $ preds graph item)
hasSource _ _ = False

hasSink :: Reticular a ra => ra -> GraphItem a -> Bool
hasSink graph item@(Vertex _) =
  (hasItem graph item) && (null $ succs graph item)
hasSink _ _ = False

hasInternal :: Reticular a ra => ra -> GraphItem a -> Bool
hasInternal g v = (hasItem g v) && (not $ hasSource g v) && (not $ hasSink g v)

hasIsolated :: Reticular a ra => ra -> GraphItem a -> Bool
hasIsolated g v = (hasItem g v) && (hasSource g v) && (hasSink g v)


instance Eq a => Reticular a (GraphItem a) where
  vertices (Vertex v) = [Vertex v]
  vertices (Edge v w) = [Vertex v, Vertex w]
  edges (Vertex v)    = []
  edges (Edge v w)    = [Edge v w]


data Graph a = Graph (Set a) (Map a [a]) (Map a [a])

instance (Ord a) => Reticular a (Graph a) where
  vertices (Graph verts _ _)        = map Vertex $ Set.toList verts
  
  succs (Graph _ _ forw) (Vertex v) = map Vertex $ forw ! v
  succs graph (Edge v w) = [Edge w $ label u | u <- succs graph (Vertex w)]
  
  preds (Graph _ back _) (Vertex v) = map Vertex $ back ! v
  preds graph (Edge v w) = [Edge (label u) v | u <- preds graph (Vertex v)]

  hasItem (Graph verts _ _) (Vertex v) =
    Set.member v verts
  hasItem g@(Graph verts _ forw) (Edge v w) =
    Set.member v verts && (elem w $ forw ! v)

instance Ord a => Eq (Graph a) where
  g == g' = edgesInOrder g    == edgesInOrder g' && 
            verticesInOrder g == verticesInOrder g'
              where edgesInOrder    = sort . edges
                    verticesInOrder = sort . vertices

instance (Ord a, Show a) => Show (Graph a) where
  show g = "graph " ++ show ((sort $ edges g) ++ sort isolatedVertices)
    where isolatedVertices = filter (hasIsolated g) $ vertices g


class Reticular a ra => EditableReticular a ra where
  insert :: ra -> GraphItem a -> ra
  delete :: ra -> GraphItem a -> ra

  (+/) :: (EditableReticular a ra, Reticular a pa) => ra -> pa -> ra
  graph +/ graph' = foldl insert graph (vertices graph' ++ edges graph')

  (-/) :: (EditableReticular a ra, Reticular a pa) => ra -> pa -> ra
  graph -/ graph' = foldl delete graph (edges graph')

instance (Ord a) => EditableReticular a (Graph a) where
  insert g@(Graph verts back forw) item@(Vertex v)
    | hasItem g item = g
    | otherwise = Graph verts' back' forw'
        where verts' = Set.insert v verts
              back'  = Map.insert v [] back
              forw'  = Map.insert v [] forw
  insert g item@(Edge v w)
    | hasItem g item = g
    | otherwise = Graph verts' back' forw'
        where (Graph verts back forw) = g `insert` Vertex v `insert` Vertex w
              verts' = verts
              back'  = Map.insertWith' (++) w [v] back
              forw'  = Map.insertWith' (++) v [w] forw

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
              back'  = Map.insertWith' (flip $ const . filter (/= v)) w [] back
              forw'  = Map.insertWith' (flip $ const . filter (/= w)) v [] forw

graph :: Ord a => [GraphItem a] -> Graph a
graph = foldl insert (Graph Set.empty Map.empty Map.empty)
