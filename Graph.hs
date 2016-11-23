module Graph where

import Data.List

type Node   = Int
type Weight = Int
data Edge   = Edge  { n1 :: Node, n2 :: Node, weight :: Weight } deriving (Eq, Show)
data Graph  = Graph { nodes:: [Node], edges :: [Edge] } deriving (Show)

-- | Total weight of all edges of a graph.
cost :: Graph -> Weight
cost = foldr (\edge acc -> acc + (weight edge)) 0 . edges

sortNodes :: Edge -> Edge
sortNodes edge@(Edge n1 n2 w)
  | n1 > n2   = Edge n2 n1 w
  | otherwise = edge

sortEdgeDesc :: Edge -> Edge -> Ordering
sortEdgeDesc e1 e2
  | n1 e1 < n1 e2 = LT
  | n1 e1 > n1 e2 = GT
  | n1 e1 == n1 e2 = if n2 e1 < n2 e2 then LT
                     else if n2 e1 > n2 e2 then GT
                     else EQ

sortEdgesDesc :: [Edge] -> [Edge]
sortEdgesDesc = sortBy sortEdgeDesc

-- | Returns a graph in a formatted string.
formatGraph :: Graph -> String
formatGraph g@(Graph v es) = mstCost ++ mstEdges
  where mstCost = show $ cost g
        mstEdges = concatMap showEdge (sortEdgesDesc $ map sortNodes es)
        showEdge e = "\n" ++ (show $ n1 e) ++ "  " ++ (show $ n2 e)

-- | Converts string to edge.
toEdge :: String -> Edge
toEdge s = Edge (nums !! 0) (nums !! 1) (nums !! 2)
  where nums = map (\w -> read w :: Int) (words s)

-- | Converts array of strings to edges.
toEdges :: [String] -> [Edge]
toEdges s = map toEdge s
--
-- | Converts a string representation of a graph into a Graph.
toGraph :: String -> Graph
toGraph str = Graph [1..numNodes] (toEdges $ tail ls)
  where ls = lines str
        numNodes = read (head ls) :: Int
