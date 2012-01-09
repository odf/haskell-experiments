import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.List (sort)
import Graph

main = defaultMain tests

data Color = Red | Green | Blue | Yellow | Magenta | Cyan | Black | White
  deriving (Eq, Ord, Show)

instance Arbitrary Color where
  arbitrary = elements [ Red, Green, Blue, Yellow, Magenta, Cyan, Black, White ]

tests = [
    testGroup "Basics" [
      testProperty "vertices"    (prop_vertices :: Graph Color -> Bool)
    , testProperty "edges"       (prop_edges    :: Graph Color -> Bool)
    , testProperty "items"       (prop_items    :: Graph Color -> Bool)
    , testProperty "adjacencies" (prop_adjs     :: Graph Color -> Bool)
    ]
  , testGroup "Containment" [
      testProperty "has item"    (prop_has_item :: 
                                     Graph Color -> GraphItem Color -> Bool)
    ]
  ]

instance (Arbitrary a) => Arbitrary (GraphItem a) where
  arbitrary = oneof [
      fmap Vertex arbitrary
    , fmap (uncurry Edge) arbitrary
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

prop_vertices g = all (`elem` verts) ends
  where verts = vertices g
        ends  = concat [map Vertex [v, w] | (Edge v w) <- edges g]

prop_edges g = all (== edgesInOrder) [succEdges, predEdges]
  where edgesInOrder = sort $ edges g
        succEdges    = sort [edge v w | v <- vertices g, w <- succs g v]
        predEdges    = sort [edge v w | w <- vertices g, v <- preds g w]
        edge v w     = Edge (Graph.label v) (Graph.label w)

prop_items g = sort (items g) == sort (vertices g ++ edges g)

prop_adjs g = all good $ vertices g
  where good v = sort (adjs g v) == sort (preds g v ++ succs g v)

prop_has_item g item = hasItem g item == (item `elem` items g)
