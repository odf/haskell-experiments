-- import some stuff

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!))
import Data.Set (Set)
import Data.List (sort)


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

data Graph a = Graph (Set a) (Map a [a]) (Map a [a]) deriving (Eq)

instance (Show a, Ord a) => Show (Graph a) where
  show g = "graph " ++ show (sort (edges g) ++ sort extraVertices)
    where extraVertices = filter (flip isolated $ g) $ vertices g


data GraphItem a = Vertex a | Edge a a deriving (Show, Ord, Eq)


empty :: (Ord a) => Graph a
empty = Graph Set.empty Map.empty Map.empty

member :: (Ord a) => GraphItem a -> Graph a -> Bool
member (Vertex v) (Graph verts _ _)  =
  Set.member v verts
member (Edge v w) g@(Graph _ _ forw) =
  (member (Vertex v) g) && (elem w $ forw ! v)

vertices :: (Ord a) => Graph a -> [GraphItem a]
vertices (Graph verts _ _) = map Vertex $ Set.toList verts

source :: (Ord a) => GraphItem a -> Graph a -> Bool
source i@(Vertex v) g@(Graph _ back _) = (member i g) && (null $ back ! v)
source _ _ = False

sink :: (Ord a) => GraphItem a -> Graph a -> Bool
sink i@(Vertex v) g@(Graph _ _ forw) = (member i g) && (null $ forw ! v)
sink _ _ = False

internal :: (Ord a) => GraphItem a -> Graph a -> Bool
internal v g = (member v g) && (not $ source v g) && (not $ sink v g)

isolated :: (Ord a) => GraphItem a -> Graph a -> Bool
isolated v g = (member v g) && (source v g) && (sink v g)

edges :: (Ord a) => Graph a -> [GraphItem a]
edges g@(Graph _ _ forw) = concat $ map edgesFrom $ vertices g
  where edgesFrom (Vertex v) = zipWith Edge (repeat v) (forw ! v)

succs :: (Ord a) => GraphItem a -> Graph a -> [GraphItem a]
succs (Vertex v) (Graph _ _ forw) = map Vertex $ forw ! v

preds :: (Ord a) => GraphItem a -> Graph a -> [GraphItem a]
preds (Vertex v) (Graph _ back _) = map Vertex $ back ! v

adjs :: (Ord a) => GraphItem a -> (Graph a) -> [GraphItem a]
adjs v g = (succs v g) ++ (preds v g)

insert :: (Ord a) => GraphItem a -> Graph a -> Graph a
insert item g
  | member item g = g
  | otherwise     = insert' item g

insert' :: (Ord a) => GraphItem a -> Graph a -> Graph a
insert' (Vertex v) g@(Graph verts back forw) =
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

delete :: (Ord a) => GraphItem a -> Graph a -> Graph a
delete item g
  | not $ member item g = g
  | otherwise           = delete' item g

delete' :: (Ord a) => GraphItem a -> Graph a -> Graph a
delete' (Vertex v) g@(Graph verts back forw) =
  let verts'    = Set.delete v verts
      back'     = Map.delete v $ without v back
      forw'     = Map.delete v $ without v forw
      without v = fmap (filter (/= v))
  in Graph verts' back' forw'
delete' (Edge v w) g@(Graph verts back forw) =
  let verts' = verts
      back'  = Map.insert w (filter (/= v) $ back ! w) back
      forw'  = Map.insert v (filter (/= w) $ forw ! v) forw
  in Graph verts' back' forw' 

graph :: (Ord a) => [GraphItem a] -> Graph a
graph = foldr insert empty


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
      let new   = filter (\v -> not $ Set.member v seen) $ adj node
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
