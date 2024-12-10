-- Define a linked list where each node has a value and a reference to the next node
data LinkedList a = Empty              
                  | Node a (LinkedList a) 
    deriving (Show, Eq) 

-- Insert an element at the beginning of the linked list
addFirst :: a -> LinkedList a -> LinkedList a
addFirst x lst = Node x lst

-- Add an element at the end of the linked list
addLast :: a -> LinkedList a -> LinkedList a
addLast x Empty = Node x Empty
addLast x (Node v next) = Node v (addLast x next)

-- Insert an element at a given position in the linked list
insertAt :: Eq a => Int -> a -> LinkedList a -> LinkedList a
insertAt n x Empty
    | n == 0    = addFirst x Empty
    | otherwise = error "Position out of bounds"
insertAt 0 x lst = addFirst x lst
insertAt n x (Node v next)
    | n < 0     = error "Negative index"
    | next == Empty && n == 1 = addLast x (Node v next)
    | otherwise = Node v (insertAt (n-1) x next)

-- Update the value of an element at a given position
updateAt :: Int -> a -> LinkedList a -> LinkedList a
updateAt _ _ Empty = error "Position out of bounds"
updateAt 0 x (Node _ next) = Node x next
updateAt n x (Node v next)
    | n < 0     = error "Negative index"
    | otherwise = Node v (updateAt (n-1) x next)

-- Retrieve the value of an element at a given position
elementAt :: Int -> LinkedList a -> a
elementAt _ Empty = error "Position out of bounds"
elementAt 0 (Node v _) = v
elementAt n (Node _ next)
    | n < 0     = error "Negative index"
    | otherwise = elementAt (n-1) next

-- Concatenate two linked lists
concatLL :: LinkedList a -> LinkedList a -> LinkedList a
concatLL Empty ys = ys
concatLL (Node v next) ys = Node v (concatLL next ys)

-- Apply a function to each element in the list
mapLL :: (a -> b) -> LinkedList a -> LinkedList b
mapLL _ Empty = Empty
mapLL f (Node v next) = Node (f v) (mapLL f next)

-- Filter elements in the list based on a predicate
filterLL :: (a -> Bool) -> LinkedList a -> LinkedList a
filterLL _ Empty = Empty
filterLL p (Node v next)
    | p v       = Node v (filterLL p next)
    | otherwise = filterLL p next

-- Reduce the list to a single value using a binary function
foldLL :: (b -> a -> b) -> b -> LinkedList a -> b
foldLL _ acc Empty = acc
foldLL f acc (Node v next) = foldLL f (f acc v) next

-- Combine two lists into a list of pairs
zipLL :: LinkedList a -> LinkedList b -> LinkedList (a, b)
zipLL Empty _ = Empty
zipLL _ Empty = Empty
zipLL (Node v1 next1) (Node v2 next2) = Node (v1, v2) (zipLL next1 next2)

-- Delete the first occurrence of an element from the linked list
delete :: (Eq a) => a -> LinkedList a -> LinkedList a
delete _ Empty = Empty
delete x (Node v next)
    | x == v    = next
    | otherwise = Node v (delete x next)

-- Check if an element is in the linked list
search :: (Eq a) => a -> LinkedList a -> Bool
search _ Empty = False
search x (Node v next)
    | x == v    = True
    | otherwise = search x next

--Convert the linked list to a list
toList :: LinkedList a -> [a]
toList Empty = []
toList (Node v next) = v : toList next

--Convert a list to a linked list
fromList :: [a] -> LinkedList a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

--length of the linked list
lengthLL :: LinkedList a -> Int
lengthLL Empty = 0
lengthLL (Node _ next) = 1 + lengthLL next

--Reverse the linked list
reverseLL :: LinkedList a -> LinkedList a
reverseLL lst = reverseHelper lst Empty
  where
    reverseHelper Empty acc = acc
    reverseHelper (Node v next) acc = reverseHelper next (Node v acc)