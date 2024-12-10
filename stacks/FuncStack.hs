-- Functional Stack Implementation
module FuncStack where

-- Define the Stack data type
data Stack a = Stack [a] deriving (Show)

-- Create an empty stack
emptyStack :: Stack a
emptyStack = Stack []

-- Push an element onto the stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- Pop an element from the stack
pop :: Stack a -> (a, Stack a)
pop (Stack [])     = error "Pop from an empty stack"
pop (Stack (x:xs)) = (x, Stack xs)

-- Peek at the top element of the stack
peek :: Stack a -> a
peek (Stack [])     = error "Peek from an empty stack"
peek (Stack (x:_))  = x

-- Map a function over the stack
mapStack :: (a -> b) -> Stack a -> Stack b
mapStack f (Stack xs) = Stack (map f xs)

-- Check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

-- Get the size of the stack
size :: Stack a -> Int
size (Stack xs) = length xs

-- Reverse the stack
reverseStack :: Stack a -> Stack a
reverseStack (Stack xs) = Stack (reverse xs)

-- Convert the stack to a list
toList :: Stack a -> [a]
toList (Stack xs) = xs

-- Create a stack from a list
fromList :: [a] -> Stack a
fromList xs = Stack xs