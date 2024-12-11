import Hashmap as HM
import Data.Maybe (isNothing)
import System.CPUTime
import Text.Printf
import Control.Monad (forM_, replicateM)
import Data.Functor ((<&>))
import Data.Maybe (isNothing, isJust)
import System.Random (randomRIO)
import Data.List (nub)
import Control.Exception (catch, SomeException)
import Data.List (foldl')

-- Timing utility
timeIt :: String -> IO a -> IO a
timeIt label action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = (fromIntegral (end - start) / (10^12)) :: Double
    printf "%s: %0.3f sec\n" label diff
    return result

-- Random string generator
randomString :: Int -> IO String
randomString len = do
    let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    sequence $ replicate len $ randomRIO (0, length chars - 1) >>= return . (chars !!)

-- Modify performance test functions for better results
testInsertPerformance :: IO ()
testInsertPerformance = do
    putStrLn "\nTesting Insert Performance:"
    forM_ [1000, 10000, 100000] $ \size -> do
        keys <- replicateM size (randomString 10)
        values <- replicateM size (randomString 10)
        let pairs = zip keys values
        timeIt (show size ++ " inserts") $ do
            let !final = foldl' (\hm (k,v) -> HM.insert k v hm) HM.empty pairs
            putStr $ "Size: " ++ show (HM.size final) ++ " "
            return ()

testLookupPerformance :: IO ()
testLookupPerformance = do
    putStrLn "\nTesting Lookup Performance:"
    -- Create smaller test data to avoid memory issues
    keys <- replicateM 10000 (randomString 10)
    values <- replicateM 10000 (randomString 10)
    let !hm = foldl' (\h (k,v) -> HM.insert k v h) HM.empty (zip keys values)
    
    forM_ [1000, 5000, 10000] $ \lookups -> do
        timeIt (show lookups ++ " lookups") $ do
            let !results = length [() | k <- take lookups (cycle keys), isJust (HM.lookup k hm)]
            putStr $ "Found: " ++ show results ++ " "
            return ()

testBulkOperations :: IO ()
testBulkOperations = do
    putStrLn "\nTesting Bulk Operations Performance:"
    forM_ [1000, 5000, 10000] $ \size -> do
        keys <- replicateM size (randomString 10)
        values <- replicateM size (randomString 10)
        let pairs = zip keys values
        putStrLn $ "\nSize " ++ show size ++ ":"
        timeIt "fromList" $ do
            let !hm = HM.fromList pairs
            putStr $ "Size: " ++ show (HM.size hm) ++ " "
            return ()
        timeIt "toList" $ do
            let !hm = HM.fromList pairs
            let !lst = HM.toList hm
            putStr $ "Length: " ++ show (length lst) ++ " "
            return ()

-- Test cases for the HashMap
testInsert :: IO ()
testInsert = do
    let hm = HM.empty
    let hm' = HM.insert "key1" "value1" hm
    if HM.lookup "key1" hm' == Just "value1" && isNothing (HM.lookup "key1" hm)
        then putStrLn "testInsert passed"
        else putStrLn "testInsert failed"

testLookup :: IO ()
testLookup = do
    let hm = HM.insert "key1" "value1" $ HM.insert "key2" "value2" HM.empty
    if HM.lookup "key1" hm == Just "value1" && HM.lookup "key2" hm == Just "value2" && isNothing (HM.lookup "key3" hm)
        then putStrLn "testLookup passed"
        else putStrLn "testLookup failed"

testDelete :: IO ()
testDelete = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    let hm' = HM.delete "key1" hm
    if isNothing (HM.lookup "key1" hm') && HM.lookup "key2" hm' == Just "value2" && HM.lookup "key1" hm == Just "value1"
        then putStrLn "testDelete passed"
        else putStrLn "testDelete failed"

testFromList :: IO ()
testFromList = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    if HM.lookup "key1" hm == Just "value1" && HM.lookup "key2" hm == Just "value2"
        then putStrLn "testFromList passed"
        else putStrLn "testFromList failed"

testToList :: IO ()
testToList = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    if HM.toList hm == [("key1", "value1"), ("key2", "value2")]
        then putStrLn "testToList passed"
        else putStrLn "testToList failed"

-- Additional tests to verify purely functional behavior
testPurelyFunctionalInsert :: IO ()
testPurelyFunctionalInsert = do
    let hm = HM.empty
    let hm' = HM.insert "key1" "value1" hm
    let hm'' = HM.insert "key2" "value2" hm
    if HM.lookup "key1" hm' == Just "value1" && isNothing (HM.lookup "key1" hm) && isNothing (HM.lookup "key2" hm)
        then putStrLn "testPurelyFunctionalInsert passed"
        else putStrLn "testPurelyFunctionalInsert failed"

testPurelyFunctionalDelete :: IO ()
testPurelyFunctionalDelete = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    let hm' = HM.delete "key1" hm
    let hm'' = HM.delete "key2" hm
    if isNothing (HM.lookup "key1" hm') && HM.lookup "key2" hm' == Just "value2" && HM.lookup "key1" hm == Just "value1"
        then putStrLn "testPurelyFunctionalDelete passed"
        else putStrLn "testPurelyFunctionalDelete failed"

-- Tests for new operations
testUpdate :: IO ()
testUpdate = do
    let hm = HM.insert "key1" "value1" HM.empty
    let hm' = HM.update "key1" "newValue1" hm
    if HM.lookup "key1" hm' == Just "newValue1" && HM.lookup "key1" hm == Just "value1"
        then putStrLn "testUpdate passed"
        else putStrLn "testUpdate failed"

testMerge :: IO ()
testMerge = do
    let hm1 = HM.fromList [("key1", "value1"), ("key2", "value2")]
    let hm2 = HM.fromList [("key2", "newValue2"), ("key3", "value3")]
    let hm' = HM.merge hm1 hm2
    if HM.lookup "key1" hm' == Just "value1" && HM.lookup "key2" hm' == Just "value2" && HM.lookup "key3" hm' == Just "value3"
        then putStrLn "testMerge passed"
        else putStrLn "testMerge failed"

testKeys :: IO ()
testKeys = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    if HM.keys hm == ["key1", "key2"]
        then putStrLn "testKeys passed"
        else putStrLn "testKeys failed"

testValues :: IO ()
testValues = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    if HM.values hm == ["value1", "value2"]
        then putStrLn "testValues passed"
        else putStrLn "testValues failed"

testSize :: IO ()
testSize = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    if HM.size hm == 2
        then putStrLn "testSize passed"
        else putStrLn "testSize failed"

testMapValues :: IO ()
testMapValues = do
    let hm = HM.fromList [("key1", "value1"), ("key2", "value2")]
    let hm' = HM.mapValues (++ "_updated") hm
    if HM.lookup "key1" hm' == Just "value1_updated" && HM.lookup "key2" hm' == Just "value2_updated"
        then putStrLn "testMapValues passed"
        else putStrLn "testMapValues failed"

-- Run all tests
main :: IO ()
main = do
    putStrLn "Running functional tests:"
    testInsert
    testLookup
    testDelete
    testFromList
    testToList
    testPurelyFunctionalInsert
    testPurelyFunctionalDelete
    testUpdate
    testMerge
    testKeys
    testValues
    testSize
    testMapValues
    
    -- Modified performance tests with error handling
    catch testInsertPerformance (\e -> putStrLn $ "Insert test error: " ++ show (e :: SomeException))
    catch testLookupPerformance (\e -> putStrLn $ "Lookup test error: " ++ show (e :: SomeException))
    catch testBulkOperations (\e -> putStrLn $ "Bulk operations test error: " ++ show (e :: SomeException))