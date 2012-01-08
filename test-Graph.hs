import Test.QuickCheck
import Graph

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
