module Hashmap (
    HashMap,
    empty,
    insert,
    lookup,
    delete,
    fromList,
    toList,
    update,
    merge,
    keys,
    values,
    size,
    mapValues
) where

import Prelude hiding (lookup)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List

-- Define the HashMap type
newtype HashMap k v = HashMap [(k, v)]
    deriving (Show, Eq)

-- Create an empty HashMap
empty :: HashMap k v
empty = HashMap []

-- Insert a key-value pair into the HashMap
insert :: Eq k => k -> v -> HashMap k v -> HashMap k v
insert key value (HashMap xs) = HashMap ((key, value) : List.filter ((/= key) . fst) xs)

-- Lookup a value by key in the HashMap
lookup :: Eq k => k -> HashMap k v -> Maybe v
lookup key (HashMap xs) = List.lookup key xs

-- Delete a key-value pair from the HashMap
delete :: Eq k => k -> HashMap k v -> HashMap k v
delete key (HashMap xs) = HashMap (List.filter ((/= key) . fst) xs)

-- Convert a list of key-value pairs to a HashMap
fromList :: Eq k => [(k, v)] -> HashMap k v
fromList = foldr (uncurry insert) empty

-- Convert a HashMap to a list of key-value pairs
toList :: HashMap k v -> [(k, v)]
toList (HashMap xs) = xs

-- Update the value associated with a key in the HashMap
update :: Eq k => k -> v -> HashMap k v -> HashMap k v
update = insert

-- Merge two HashMaps
merge :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
merge (HashMap xs) (HashMap ys) = HashMap (xs ++ List.filter (\(k, _) -> k `notElem` List.map fst xs) ys)

-- Retrieve a list of all keys in the HashMap
keys :: HashMap k v -> [k]
keys (HashMap xs) = List.map fst xs

-- Retrieve a list of all values in the HashMap
values :: HashMap k v -> [v]
values (HashMap xs) = List.map snd xs

-- Get the number of key-value pairs in the HashMap
size :: HashMap k v -> Int
size (HashMap xs) = length xs

-- Apply a function to all values in the HashMap
mapValues :: (v -> v') -> HashMap k v -> HashMap k v'
mapValues f (HashMap xs) = HashMap (List.map (Bifunctor.second f) xs)
