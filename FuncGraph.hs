module FuncGraph where

-- Define the Graph data type
data Graph a = Graph [(a, [a])] deriving (Show)

-- Function to create an empty graph
emptyGraph :: Graph a
emptyGraph = Graph []

-- Function to add a vertex to the graph
addVertex :: Eq a => a -> Graph a -> Graph a
addVertex v (Graph g) = Graph ((v, []):g)

-- Function to add an edge to the graph
addEdge :: Eq a => a -> a -> Graph a -> Graph a
addEdge v1 v2 (Graph g) = Graph (map addEdge' g)
  where
    addEdge' (v, adj) 
      | v == v1 = (v, v2:adj)
      | otherwise = (v, adj)

-- Function to get the vertices of the graph
vertices :: Graph a -> [a]
vertices (Graph g) = map fst g

-- Function to get the edges of the graph
edges :: Graph a -> [(a, a)]
edges (Graph g) = [(v, u) | (v, adj) <- g, u <- adj]

-- Function to check if a vertex is in the graph
vertexExists :: Eq a => a -> Graph a -> Bool
vertexExists v (Graph g) = any (\(x, _) -> x == v) g

-- Function to check if an edge is in the graph
edgeExists :: Eq a => a -> a -> Graph a -> Bool
edgeExists v1 v2 (Graph g) = any (\(v, adj) -> v == v1 && v2 `elem` adj) g
