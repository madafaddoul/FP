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


