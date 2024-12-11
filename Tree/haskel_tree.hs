module HaskelTree where
-- Define a binary tree where each node has a value and two subtrees (left and right)
data Tree a = Empty            
            | Node a (Tree a) (Tree a)  
    deriving (Show, Eq) 

-- Insert an element into the binary tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty 
insert x (Node v left right)
    | x < v     = Node v (insert x left) right 
    | x > v     = Node v left (insert x right) 
    | otherwise = Node v left right    


-- Check if an element is in the binary tree
search :: (Ord a) => a -> Tree a -> Bool
search x Empty = False 
search x (Node v left right)
    | x == v    = True  
    | x < v     = search x left 
    | otherwise = search x right 



-- Inorder traversal (Left, Root, Right)
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node v left right) = inorder left ++ [v] ++ inorder right

-- Preorder traversal (Root, Left, Right)
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node v left right) = [v] ++ preorder left ++ preorder right

-- Postorder traversal (Left, Right, Root)
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node v left right) = postorder left ++ postorder right ++ [v]


-- Find the maximum value in a binary search tree
findMax :: Tree a -> a
findMax (Node v _ Empty) = v 
findMax (Node _ _ right) = findMax right 
findMax Empty = error "Tree is empty, no maximum value" 


-- Find the minimum value in a binary search tree
findMin :: Tree a -> a
findMin (Node v Empty _) = v 
findMin (Node _ left _)  = findMin left 
findMin Empty = error "Tree is empty, no minimum value" 


-- Delete an element from the binary tree
delete :: (Ord a) => a -> Tree a -> Tree a
delete x Empty = Empty 
delete x (Node v left right)
    | x < v     = Node v (delete x left) right 
    | x > v     = Node v left (delete x right) 
    | otherwise = -- If x matches the current node's value, delete this node
        case (left, right) of
            (Empty, Empty) -> Empty -- No children, delete the node
            (left, Empty)  -> left  -- Only left child, replace node with left child
            (Empty, right) -> right -- Only right child, replace node with right child
            (left, right)  -> -- Node has two children
                let minVal = findMin right -- Find the smallest value in the right subtree
                in Node minVal left (delete minVal right) -- Replace current node with minVal

-- Calculate the height of the tree
height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Map a function over the binary tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node v left right) = Node (f v) (mapTree f left) (mapTree f right)



-- Fold the binary tree to a single value using a binary function
foldTree :: (b -> a -> b) -> b -> Tree a -> b
foldTree _ acc Empty = acc
foldTree f acc (Node v left right) = foldTree f (f (foldTree f acc left) v) right