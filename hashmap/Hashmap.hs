module Hashmap (
    HashMap,
    empty,
    insert,
    lookup,
    delete,
    fromList,
    toList
) where

import Prelude hiding (lookup)
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
