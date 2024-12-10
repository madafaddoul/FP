-- FuncQueueTest.hs
import FuncQueue
import Data.Time

-- Test the functional queue implementation
main :: IO ()
main = do
    start <- getCurrentTime

    let queue1 = emptyQueue
    let queue2 = enqueue 10 queue1
    let queue3 = enqueue 20 queue2
    print $ "Queue after enqueuing 10 and 20: " ++ show queue3
    print $ "Is queue3 empty? " ++ show (isEmpty queue3)
    
    let frontElement = peek queue3
    print $ "Front element (peek): " ++ show frontElement
    
    let (dequeuedElement, queue4) = dequeue queue3
    print $ "Dequeued element: " ++ show dequeuedElement
    print $ "Queue after dequeuing: " ++ show queue4
    print $ "Is queue4 empty? " ++ show (isEmpty queue4)
    
    -- Additional tests
    let queue5 = enqueue 30 queue4
    let queue6 = enqueue 40 queue5
    print $ "Queue after enqueuing 30 and 40: " ++ show queue6

    let queue7 = reverseQueue queue6
    print $ "Queue after reversing: " ++ show queue7

    let list = [1, 2, 3]
    let queue8 = fromList list
    print $ "Queue from list [1, 2, 3]: " ++ show queue8
    print $ "Queue to list: " ++ show (toList queue8)

    let queue9 = mapQueue (+1) queue8
    print $ "Queue after mapping (+1): " ++ show queue9

    end <- getCurrentTime
    let diff = diffUTCTime end start
    print $ "Time taken: " ++ show diff