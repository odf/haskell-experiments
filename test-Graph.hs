import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.List (sort)
import Graph

main = defaultMain tests

tests = [
  testGroup "Basics" [
      testProperty "vertices" (prop_vertices :: Graph Int -> Bool)
    , testProperty "edges"    (prop_edges    :: Graph Int -> Bool)
    ]
  ]

instance (Ord a, Arbitrary a) => Arbitrary (Graph a) where
  arbitrary = do
    verts <- listOf arbitrary
    let g =  graph $ map Vertex verts
    let allEdges = [Edge v w | v <- verts, w <- verts]
    if (null allEdges)
      then return g
      else do
        edges <- listOf $ elements allEdges
      	return (foldl insert g edges)

prop_vertices :: (Ord a) => Graph a -> Bool
prop_vertices g = all (`elem` verts) ends
  where verts = vertices g
        ends  = concat [map Vertex [v, w] | (Edge v w) <- edges g]

prop_edges :: (Ord a) => Graph a -> Bool
prop_edges g = all (== edgesInOrder) [succEdges, predEdges]
  where edgesInOrder = sort $ edges g
        succEdges    = sort [edge v w | v <- vertices g, w <- succs g v]
        predEdges    = sort [edge v w | w <- vertices g, v <- preds g w]
        edge v w     = Edge (Graph.label v) (Graph.label w)

