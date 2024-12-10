-- Define a binary tree where each node has a value and two subtrees (left and right)
data Tree a = Empty               -- An empty tree 
            | Node a (Tree a) (Tree a)  -- A node that contains a value and two subtrees (left and right)
    deriving (Show, Eq) -- Allow us to print and compare trees

-- Insert an element into the binary tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty -- If the tree is empty, create a new node
insert x (Node v left right)
    | x < v     = Node v (insert x left) right -- if value>rootValue insert in the left subtree
    | x > v     = Node v left (insert x right) -- if value<rootValue insert in the right subtree
    | otherwise = Node v left right           -- Do nothing if the value already exists


-- Check if an element is in the binary tree
search :: (Ord a) => a -> Tree a -> Bool
search x Empty = False -- If the tree is empty, return False
search x (Node v left right)
    | x == v    = True  -- If the value matches, return True
    | x < v     = search x left -- If the value is less, search in the left subtree
    | otherwise = search x right -- Otherwise, search in the right subtree



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
findMax (Node v _ Empty) = v -- If there is no right child, return the value of the current node
findMax (Node _ _ right) = findMax right -- Move to the right subtree to find the maximum
findMax Empty = error "Tree is empty, no maximum value" -- Handle the case for an empty tree


-- Find the minimum value in a binary search tree
findMin :: Tree a -> a
findMin (Node v Empty _) = v -- If there is no left child, this is the smallest value
findMin (Node _ left _)  = findMin left -- Keep moving to the left subtree
findMin Empty = error "Tree is empty, no minimum value" -- Handle the case for an empty tree


-- Delete an element from the binary tree
delete :: (Ord a) => a -> Tree a -> Tree a
delete x Empty = Empty -- If the tree is empty, return an empty tree
delete x (Node v left right)
    | x < v     = Node v (delete x left) right -- If x is less, delete from the left subtree
    | x > v     = Node v left (delete x right) -- If x is greater, delete from the right subtree
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