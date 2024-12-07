-- FuncStack.hs
module FuncStack where

-- Functional Stack Implementation
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
