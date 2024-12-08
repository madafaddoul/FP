-- func_queue_test.hs
import FuncQueue

-- Test the functional queue implementation
main :: IO ()
main = do
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
    
    let (dequeuedElement2, queue7) = dequeue queue6
    print $ "Dequeued element: " ++ show dequeuedElement2
    print $ "Queue after dequeuing: " ++ show queue7
    
    let (dequeuedElement3, queue8) = dequeue queue7
    print $ "Dequeued element: " ++ show dequeuedElement3
    print $ "Queue after dequeuing: " ++ show queue8
    
    let (dequeuedElement4, queue9) = dequeue queue8
    print $ "Dequeued element: " ++ show dequeuedElement4
    print $ "Is queue9 empty? " ++ show (isEmpty queue9)