import FuncGraph
import System.CPUTime
import Text.Printf

timeIt :: IO a -> IO a
timeIt action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return result

main :: IO ()
main = do
    -- Test small graph (original tests)
    putStrLn "\nTesting small graph operations:"
    timeIt $ do
        let g = emptyGraph
        let g1 = addVertex 1 g
        let g2 = addVertex 2 g1
        let g3 = addEdge 1 2 g2

        -- Test basic operations
        print $ vertices g3    -- Should print [2,1]
        print $ edges g3       -- Should print [(1,2)]
        print $ vertexExists 1 g3  -- Should print True
        print $ vertexExists 3 g3  -- Should print False
        print $ edgeExists 1 2 g3  -- Should print True
        print $ edgeExists 2 1 g3  -- Should print False

        -- Additional test cases
        let g4 = addVertex 3 g3
        let g5 = addEdge 2 3 g4
        print $ edges g5       -- Should print [(1,2),(2,3)]
        print $ findPath 1 3 g5  -- Should print Just [1,2,3]

        -- Test new operations
        print $ neighbors 1 g5  -- Should print [2]
        print $ inDegree 2 g5  -- Should print 1
        print $ outDegree 2 g5 -- Should print 1
        print $ hasCycle g5    -- Should print False
        
        -- Create a cycle and test
        let g6 = addEdge 3 1 g5
        print $ hasCycle g6    -- Should print True
        
        -- Test graph merging
        let g7 = addVertex 4 emptyGraph
        let g8 = mergeGraphs g5 g7
        print $ vertices g8    -- Should print [4,3,2,1]
        
        -- Test graph info
        print $ graphInfo g8   -- Should print (4,2)
        print $ isEmpty g8     -- Should print False
        
        -- Test all paths
        print $ getAllPaths 1 3 g5  -- Should print [[1,2,3]]
        print $ isConnected g5      -- Should print True
    
    -- Test medium graph (100 vertices)
    putStrLn "\nTesting medium graph operations:"
    timeIt $ do
        let mediumGraph = foldr addVertex emptyGraph [1..100]
        let mediumGraphWithEdges = foldr (\i g -> addEdge i (i+1) g) mediumGraph [1..99]
        print $ graphInfo mediumGraphWithEdges
        print $ findPath 1 100 mediumGraphWithEdges
        print $ hasCycle mediumGraphWithEdges
        print $ isConnected mediumGraphWithEdges
    
    -- Test large graph (1000 vertices)
    putStrLn "\nTesting large graph operations:"
    timeIt $ do
        let largeGraph = foldr addVertex emptyGraph [1..1000]
        let largeGraphWithEdges = foldr (\i g -> addEdge i (i+1) g) largeGraph [1..999]
        print $ graphInfo largeGraphWithEdges
        print $ findPath 1 1000 largeGraphWithEdges
        print $ hasCycle largeGraphWithEdges
        print $ isConnected largeGraphWithEdges
    
    -- Test very large graph (5000 vertices)
    putStrLn "\nTesting very large graph operations:"
    timeIt $ do
        let veryLargeGraph = foldr addVertex emptyGraph [1..5000]
        let veryLargeGraphWithEdges = foldr (\i g -> addEdge i (i+1) g) veryLargeGraph [1..4999]
        print $ graphInfo veryLargeGraphWithEdges
        print $ findPath 1 5000 veryLargeGraphWithEdges
        print $ hasCycle veryLargeGraphWithEdges
        print $ isConnected veryLargeGraphWithEdges
    
    -- Test complex operations on medium graph
    putStrLn "\nTesting complex operations on medium graph:"
    timeIt $ do
        let g = foldr addVertex emptyGraph [1..100]
        let g' = foldr (\(x,y) g -> addEdge x y g) g [(x,y) | x <- [1..50], y <- [51..100], x `mod` 5 == 0]
        print $ graphInfo g'
        print $ length $ getAllPaths 1 100 g'
        print $ neighbors 25 g'
        print $ outDegree 25 g'