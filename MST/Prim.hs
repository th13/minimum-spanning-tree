module MST.Prim where

import Data.List
import Data.Maybe
import Graph

-- | Determine if edge crosses a cut of the graph.
crossesCut :: [Node] -> [Node] -> Edge -> Bool
crossesCut v s edge
  | elem (n1 edge) s && elem (n2 edge) cut = True
  | elem (n2 edge) s && elem (n1 edge) cut = True
  | otherwise                              = False
  where cut = v \\ s

-- | Return list of edges that cross over a cut between V and S.
crossingCut :: [Node] -> [Node] -> [Edge] -> [Edge]
crossingCut v s = filter (crossesCut v s)

-- | Select lightest edge between vertices in S and V \ S
lightestEdge :: [Node] -> [Node] -> [Edge] -> Maybe Edge
lightestEdge v s edges
  | v \\ s == []     = Nothing
  | crossEdges == [] = Just $ Edge (-1) 0 0        -- Hack to identify when we have an impossible graph.
  | otherwise        = return $ foldr1 lighterEdge crossEdges
  where crossEdges = crossingCut v s edges
        lighterEdge e1 e2
          | weight e1 < weight e2 = e1
          | otherwise             = e2

-- | Returns an MST of a graph.
mst :: Graph -> Maybe Graph
mst g = mst' g [head $ nodes g] []
  where mst' g@(Graph v es) s a
          | isNothing le = Just $ Graph (sort . nub $ s) a
          | n1 (fromJust le) == -1  = Nothing       -- Impossible to form MST.
          | otherwise    = mst' g s' a'
          where le = lightestEdge v s es
                s' = s ++ [(n2 $ fromJust le), (n1 $ fromJust le)]
                a' = a ++ [(fromJust le)]
