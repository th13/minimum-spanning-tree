module MST.Kruskal where

import Data.List
import Graph

createSet :: a -> [[a]] -> [[a]]
createSet u = ([u] :)

findSet :: Eq a => a -> [[a]] -> Int
findSet u sets = findSet' u sets 0
  where findSet' u sets n
          | elem u (head sets) = n
          | otherwise   = findSet' u (tail sets) (n+1)

unionSets :: Eq a => a -> a -> [[a]] -> [[a]]
unionSets u v sets
  | uN == vN = sets
  | otherwise = sets'
  where uN = findSet u sets
        vN = findSet v sets
        uSet = sets !! uN
        vSet = sets !! vN
        sets'  = ((sets \\ [uSet]) \\ [vSet]) ++ [uSet ++ vSet]

sortEdge :: Edge -> Edge -> Ordering
sortEdge e1 e2
  | weight e1 < weight e2 = LT
  | weight e1 > weight e2 = GT
  | otherwise             = EQ

sortEdges :: [Edge] -> [Edge]
sortEdges = sortBy sortEdge

mst :: Graph -> Maybe Graph
mst g = mst' g [] (sortEdges $ edges g) (map (: []) (nodes g))
  where mst' g a f assoc
          | f == []   = if (length assoc) == 1
                          then return $ Graph (nodes g) a
                          else Nothing
          | otherwise = mst' g a' f' assoc'
          where minEdge = head f
                uN = findSet (n1 minEdge) assoc
                vN = findSet (n2 minEdge) assoc
                a'
                  | uN /= vN  = minEdge : a
                  | otherwise = a
                f' = tail f
                assoc' = unionSets (n1 minEdge) (n2 minEdge) assoc
