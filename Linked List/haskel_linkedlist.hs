-- Define a linked list where each node has a value and a reference to the next node
data LinkedList a = Empty                -- An empty list
                  | Node a (LinkedList a) -- A node that contains a value and a reference to the next node
    deriving (Show, Eq) -- Allow us to print and compare lists

-- Insert an element at the beginning of the linked list
insert :: a -> LinkedList a -> LinkedList a
insert x lst = Node x lst

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

-- Convert the linked list to a list
toList :: LinkedList a -> [a]
toList Empty = []
toList (Node v next) = v : toList next

-- Convert a list to a linked list
fromList :: [a] -> LinkedList a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

-- Calculate the length of the linked list
lengthLL :: LinkedList a -> Int
lengthLL Empty = 0
lengthLL (Node _ next) = 1 + lengthLL next

-- Reverse the linked list
reverseLL :: LinkedList a -> LinkedList a
reverseLL lst = reverseHelper lst Empty
  where
    reverseHelper Empty acc = acc
    reverseHelper (Node v next) acc = reverseHelper next (Node v acc)