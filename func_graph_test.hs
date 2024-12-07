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
    timeIt $ do
        let g = emptyGraph
        let g1 = addVertex 1 g
        let g2 = addVertex 2 g1
        let g3 = addEdge 1 2 g2

        print $ vertices g3 -- Should print [1, 2]
        print $ edges g3    -- Should print [(1, 2)]
        print $ vertexExists 1 g3 -- Should print True
        print $ vertexExists 3 g3 -- Should print False
        print $ edgeExists 1 2 g3 -- Should print True
        print $ edgeExists 2 1 g3 -- Should print False