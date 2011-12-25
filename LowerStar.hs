import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!))
import Data.Set (Set, member, notMember)
import Data.List (sort)


class AComplex c where
  cells   :: Ord a => c a -> [a]
  facesOf :: Ord a => a -> c a -> [a]


type Cells a = [a]
type Boundary a = Map a [a]

data Complex a = Complex (Cells a) (Boundary a)

instance AComplex Complex where
  cells (Complex cells _)     = cells
  facesOf cell (Complex _ bd) = bd ! cell


data Subcomplex a = Subcomplex (Complex a) (Set a)

instance AComplex Subcomplex where
  
  cells sc@(Subcomplex c excluded) = 
    filter (flip notMember $ excluded) $ cells c
  
  facesOf cell sc@(Subcomplex c _) = 
    filter (flip notMember $ excluded) $ facesOf cell c


firstWithNeighborCount :: (Ord a, AComplex c) => Int -> c a -> a
firstWithNeighborCount n c = head $ filter good (cells c)
  where good = (n==) . length . (flip facesOf $ c)
