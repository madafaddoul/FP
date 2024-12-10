import Hashmap as HM
import Data.Maybe (isNothing)

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

-- Run all tests
main :: IO ()
main = do
    testInsert
    testLookup
    testDelete
    testFromList
    testToList
    testPurelyFunctionalInsert
    testPurelyFunctionalDelete