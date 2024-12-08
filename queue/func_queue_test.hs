-- func_queue_test.hs
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

    end <- getCurrentTime
    let diff = diffUTCTime end start
    print $ "Time taken: " ++ show diff