
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

-- Apply a function to each element in the list: fmap for linked lists
mapLL :: (a -> b) -> LinkedList a -> LinkedList b
mapLL _ Empty = Empty
mapLL f (Node v next) = Node (f v) (mapLL f next)

-- Filter elements in the list based on a predicate
filterLL :: (a -> Bool) -> LinkedList a -> LinkedList a
filterLL _ Empty = Empty
filterLL p (Node v next)
    | p v       = Node v (filterLL p next)
    | otherwise = filterLL p next

--Reduce the list to a single value using a binary function
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

-- Combine two lists using a binary function
zipWithLL :: (a -> b -> c) -> LinkedList a -> LinkedList b -> LinkedList c
zipWithLL _ Empty _ = Empty
zipWithLL _ _ Empty = Empty
zipWithLL f (Node v1 next1) (Node v2 next2) = Node (f v1 v2) (zipWithLL f next1 next2) 




main :: IO ()
main = do
    -- Test addFirst
    let list1 = addFirst 1 Empty
    print $ "Add first 1 to empty list: " ++ show list1

    -- Test addLast
    let list2 = addLast 2 list1
    print $ "Add last 2 to list: " ++ show list2

    -- Test insertAt
    let list3 = insertAt 1 3 list2
    print $ "Insert 3 at position 1: " ++ show list3

    -- Test updateAt
    let list4 = updateAt 1 4 list3
    print $ "Update position 1 to 4: " ++ show list4

    -- Test elementAt
    let elem = elementAt 1 list4
    print $ "Element at position 1: " ++ show elem

    -- Test concatLL
    let list5 = concatLL list4 list1
    print $ "Concatenate lists: " ++ show list5

    -- Test mapLL
    let list6 = mapLL (+1) list5
    print $ "Map (+1) over list: " ++ show list6

    -- Test filterLL
    let list7 = filterLL (>2) list6
    print $ "Filter elements > 2: " ++ show list7

    -- Test foldLL
    let sum = foldLL (+) 0 list7
    print $ "Sum of elements: " ++ show sum

    -- Test zipLL
    let list8 = zipLL list7 list6
    print $ "Zip lists: " ++ show list8

    -- Test delete
    let list9 = delete 3 list7
    print $ "Delete first occurrence of 3: " ++ show list9

    -- Test search
    let found = search 4 list7
    print $ "Search for 4: " ++ show found

    -- Test toList
    let list10 = toList list7
    print $ "Convert to list: " ++ show list10

    -- Test fromList
    let list11 = fromList [5, 6, 7]
    print $ "Convert from list [5, 6, 7]: " ++ show list11

    -- Test lengthLL
    let len = lengthLL list7
    print $ "Length of list: " ++ show len

    -- Test reverseLL
    let list12 = reverseLL list7
    print $ "Reverse list: " ++ show list12

    -- Test zipWithLL
    let list13 = zipWithLL (+) list7 list6
    print $ "ZipWith (+) lists: " ++ show list13