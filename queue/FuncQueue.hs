-- FuncQueue.hs
module FuncQueue where

-- Functional Queue Implementation using two stacks
data Queue a = Queue [a] [a] deriving (Show)

-- Create an empty queue
emptyQueue :: Queue a
emptyQueue = Queue [] []

-- Enqueue an element
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = Queue front (x:back)

-- Dequeue an element
dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "Dequeue from an empty queue"
dequeue (Queue [] back) = dequeue (Queue (reverse back) [])
dequeue (Queue (x:xs) back) = (x, Queue xs back)

-- Peek at the front element
peek :: Queue a -> a
peek (Queue [] []) = error "Peek from an empty queue"
peek (Queue [] back) = head (reverse back)
peek (Queue (x:_) _) = x

-- Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False