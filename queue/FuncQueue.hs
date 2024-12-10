-- FuncQueue.hs
module FuncQueue where

-- Define the Queue data type
data Queue a = Queue [a] deriving (Show)

-- Create an empty queue
emptyQueue :: Queue a
emptyQueue = Queue []

-- Enqueue an element onto the queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

-- Dequeue an element from the queue
dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [])     = error "Dequeue from an empty queue"
dequeue (Queue (x:xs)) = (x, Queue xs)

-- Peek at the front element of the queue
peek :: Queue a -> a
peek (Queue [])     = error "Peek from an empty queue"
peek (Queue (x:_))  = x

-- Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue xs) = null xs

-- Get the size of the queue
size :: Queue a -> Int
size (Queue xs) = length xs

-- Reverse the queue
reverseQueue :: Queue a -> Queue a
reverseQueue (Queue xs) = Queue (reverse xs)

-- Convert the queue to a list
toList :: Queue a -> [a]
toList (Queue xs) = xs

-- Create a queue from a list
fromList :: [a] -> Queue a
fromList xs = Queue xs

-- Map a function over the queue
mapQueue :: (a -> b) -> Queue a -> Queue b
mapQueue f (Queue xs) = Queue (map f xs)