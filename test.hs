-- import some stuff

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!))
import Data.Set (Set, member)
import Data.List (sort)

import Control.Applicative


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


-- a data type for directed graphs (incomplete)

type AdjMap a = Map a [a]

data Graph a = Graph (Set a) (AdjMap a) (AdjMap a) deriving (Eq)

instance (Show a, Ord a) => Show (Graph a) where
  show g = "graph " ++ (show $ sort $ edges g)


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

edge :: (Ord a) => (a, a) -> (Graph a) -> Bool
edge (v, w) g@(Graph _ _ forw) = (vertex v g) && (elem w $ forw ! v)

succs :: (Ord a) => a -> (Graph a) -> [a]
succs v (Graph _ _ forw) = forw ! v

preds :: (Ord a) => a -> (Graph a) -> [a]
preds v (Graph _ back _) = back ! v

adjs :: (Ord a) => a -> (Graph a) -> [a]
adjs v g = (succs v g) ++ (preds v g)

edges :: (Ord a) => (Graph a) -> [(a, a)]
edges (Graph verts _ forw) = concat $ map edgesFrom $ Set.toList verts
  where edgesFrom = zip <$> repeat <*> (forw !)

plusVertex :: (Ord a) => a -> (Graph a) -> (Graph a)
plusVertex v g@(Graph verts back forw)
  | vertex v g = g
  | otherwise  =
    let verts' = Set.insert v verts
        back'  = Map.insert v [] back
        forw'  = Map.insert v [] forw
    in Graph verts' back' forw'
       
minusVertex :: (Ord a) => a -> (Graph a) -> (Graph a)
minusVertex v g@(Graph verts back forw)
  | not $ vertex v g = g
  | otherwise  =
    let verts' = Set.delete v verts
        back'  = Map.delete v $ without v back
        forw'  = Map.delete v $ without v forw
        without = \v -> fmap (filter (/= v))
    in Graph verts' back' forw'

plusEdge :: (Ord a) => (a, a) -> (Graph a) -> (Graph a)
plusEdge (v, w) g
  | edge (v, w) g = g
  | otherwise  =
    let (Graph verts back forw) = foldr plusVertex g [v, w]
        verts' = verts
        back'  = Map.insert w (back ! w ++ [v]) back
        forw'  = Map.insert v (forw ! v ++ [w]) forw
    in Graph verts' back' forw'

minusEdge :: (Ord a) => (a, a) -> (Graph a) -> (Graph a)
minusEdge (v, w) g@(Graph verts back forw)
  | not $ edge (v, w) g = g
  | otherwise  =
    let verts' = verts
        back'  = Map.insert w (filter (/= v) $ back ! w) back
        forw'  = Map.insert v (filter (/= w) $ forw ! v) forw
    in Graph verts' back' forw' 

graph :: (Ord a) => [(a, a)] -> (Graph a)
graph as = foldr plusEdge (Graph Set.empty Map.empty Map.empty) as


-- Chris Okasaki's persistent real-time queue

data Queue a = Queue [a] [a] [a] deriving (Show)

queue :: [a] -> [a] -> [a] -> Queue a
queue front rear (x:xs) = Queue front rear xs
queue [] [] []          = Queue [] [] []
queue front rear []     = let front' = rotate front rear []
                          in Queue front' [] front'

rotate :: [a] -> [a] -> [a] -> [a]
rotate (f:fs) (r:rs) a = f : rotate fs rs (r : a)
rotate []     (r:rs) a = r : a

push :: a -> Queue a -> Queue a
push x (Queue front rear schedule) = queue front (x : rear) schedule

first :: Queue a -> Maybe a
first (Queue (f:fs) _ _) = Just f
first _ = Nothing

rest :: Queue a -> Queue a
rest (Queue (f:fs) rear schedule) = queue fs rear schedule
rest (Queue [] rear schedule) = queue [] rear schedule


-- Generic graph traversal.

class TodoBag b where
  pushTodo  :: a   -> b a -> b a
  firstTodo :: b a -> Maybe a
  restTodo  :: b a -> b a

instance TodoBag [] where
  pushTodo         = (:)
  firstTodo []     = Nothing
  firstTodo (x:xs) = Just x
  restTodo  []     = []
  restTodo  (x:xs) = xs

instance TodoBag Queue where
  pushTodo  = push
  firstTodo = first
  restTodo  = rest


traversal :: (Ord a, TodoBag b) => (a -> [a]) -> Set a -> b a -> [a]
traversal adj seen todo =
  case (firstTodo todo) of
    Nothing   -> []
    Just node ->
      let new   = filter (\v -> not $ member v seen) $ adj node
          todo' = foldr pushTodo (restTodo todo) new
          seen' = foldr Set.insert seen (node : new)
      in node : traversal adj seen' todo'

dfs :: Ord a => (a -> [a]) -> [a] -> [a]
dfs adj sources = traversal adj Set.empty sources

bfs :: Ord a => (a -> [a]) -> [a] -> [a]
bfs adj sources = traversal adj Set.empty todo   
  where todo = foldl (flip push) (queue [] [] []) sources

type VorE a = Either a (a,a)

liftAdj :: (a -> [a]) -> (VorE a -> [VorE a])
liftAdj adj (Left v) = map Right $ zip (repeat v) (adj v)
liftAdj adj (Right (u, v)) = liftAdj adj $ Left v

byEdges ::((VorE a -> [VorE a]) -> [VorE a] -> [VorE a])
          -> (a -> [a]) -> [a] -> [VorE a] 
byEdges method adj sources = method (liftAdj adj) $ map Left sources


-- Computation of flow pairs and singular nodes in an ordered, directed graph

flow :: Ord a => Map a [a] -> [a] -> [Either a (a,a)]
flow adj ranked = flow' candidates adj ranked
  where candidates = filterByNeighborCount 1 adj ranked

flow' :: Ord a => [a] -> Map a [a] -> [a] -> [Either a (a,a)]
flow' (c : cs) adj ranked = Right (c, d) : flow adj' ranked'
  where d       = head . (adj !) $ c
        adj'    = withoutNodes [c, d] adj
        ranked' = filter (`notElem` [c, d]) ranked
flow' [] adj ranked = flow'' candidates adj ranked
  where candidates = filterByNeighborCount 0 adj ranked

flow'' :: Ord a => [a] -> Map a [a] -> [a] -> [Either a (a,a)]
flow'' (c : cs) adj ranked = Left c : flow adj' ranked'
  where adj'    = withoutNodes [c] adj
        ranked' = filter (/= c) ranked
flow'' [] _ _ = []

filterByNeighborCount :: Ord a => Int -> Map a [a] -> [a] -> [a]
filterByNeighborCount n adj = filter $ (==n) . length . getAdj
  where getAdj = flip (Map.findWithDefault []) $ adj 

withoutNodes :: Ord a => [a] -> Map a [a] -> Map a [a]
withoutNodes nodes adj = 
  fmap (filter (`notElem` nodes)) $ foldr Map.delete adj nodes
