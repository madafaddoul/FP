module FuncGraph where
import Data.List (nub)

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
addEdge v1 v2 (Graph g) = 
  if vertexExists v1 (Graph g) && vertexExists v2 (Graph g)
  then Graph (map addEdge' g)
  else Graph g
  where
  addEdge' (v, adj) 
    | v == v1 && notElem v2 adj = (v, v2:adj)
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
edgeExists v1 v2 (Graph g) = 
  case lookup v1 g of
    Just adj -> v2 `elem` adj
    Nothing -> False

-- Function to remove a vertex from the graph
removeVertex :: Eq a => a -> Graph a -> Graph a
removeVertex v (Graph g) = Graph [(x, filter (/= v) adj) | (x, adj) <- filter ((/= v) . fst) g]

-- Function to remove an edge from the graph
removeEdge :: Eq a => a -> a -> Graph a -> Graph a
removeEdge v1 v2 (Graph g) = Graph (map removeEdge' g)
  where
  removeEdge' (v, adj)
    | v == v1 = (v, filter (/= v2) adj)
    | otherwise = (v, adj)

-- Modified findPath to maintain correct order
findPath :: Eq a => a -> a -> Graph a -> Maybe [a]
findPath start end graph = dfs [start] []
  where
    dfs [] _ = Nothing
    dfs (x:xs) visited
      | x == end = Just [end, start]  -- Initialize with end and start
      | x `elem` visited = dfs xs visited
      | otherwise = case dfs (adjacent x ++ xs) (x:visited) of
                     Nothing -> dfs xs visited
                     Just path -> Just (if head path == end 
                                      then path 
                                      else x:path)
    adjacent v = case lookup v (let (Graph g) = graph in g) of
                  Nothing -> []
                  Just adj -> adj

-- Function to get neighbors of a vertex
neighbors :: Eq a => a -> Graph a -> [a]
neighbors v (Graph g) = case lookup v g of
  Just adj -> adj
  Nothing -> []

-- Function to get in-degree of a vertex
inDegree :: Eq a => a -> Graph a -> Int
inDegree v (Graph g) = length [(v1, v2) | (v1, adj) <- g, v2 <- adj, v2 == v]

-- Function to get out-degree of a vertex
outDegree :: Eq a => a -> Graph a -> Int
outDegree v (Graph g) = length $ neighbors v (Graph g)

-- Function to check if graph has cycles
hasCycle :: Eq a => Graph a -> Bool
hasCycle graph@(Graph g) = any (\v -> not $ null $ findCycle v [] []) (vertices graph)
  where
  findCycle v visited stack
    | v `elem` stack = [v]
    | v `elem` visited = []
    | otherwise = concat [if null cycle then [] else v:cycle
              | next <- neighbors v graph
              , let cycle = findCycle next visited (v:stack)]

-- Function to merge two graphs
mergeGraphs :: Eq a => Graph a -> Graph a -> Graph a
mergeGraphs (Graph g1) (Graph g2) = Graph $ foldr addEntry g1 g2
  where
  addEntry (v, adj) acc = case lookup v acc of
    Just existing -> (v, union adj existing) : filter ((/= v) . fst) acc
    Nothing -> (v, adj) : acc
  union xs ys = xs ++ filter (`notElem` xs) ys

-- Function to get graph size information
graphInfo :: Graph a -> (Int, Int)
graphInfo g = (length $ vertices g, length $ edges g)

-- Function to check if graph is empty
isEmpty :: Graph a -> Bool
isEmpty (Graph g) = null g

-- Modified getAllPaths for correct path order
getAllPaths :: Eq a => a -> a -> Graph a -> [[a]]
getAllPaths start end graph = findAllPaths start end [] []
  where
    findAllPaths curr target visited paths
      | curr == target = [reverse (curr:visited)]
      | curr `elem` visited = []
      | otherwise = concat [findAllPaths next target (curr:visited) paths 
                          | next <- neighbors curr graph]

-- Modified isConnected to fix connectivity check
isConnected :: Eq a => Graph a -> Bool
isConnected g@(Graph []) = True
isConnected g@(Graph g') = 
    let start = fst (head g')
        reachable = findReachable start []
    in length reachable == length (vertices g)
  where
    findReachable curr visited 
      | curr `elem` visited = visited
      | otherwise = foldr addReachable (curr:visited) (neighbors curr g)
    addReachable next acc = findReachable next acc
