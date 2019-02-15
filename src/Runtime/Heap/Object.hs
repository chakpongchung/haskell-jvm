module Runtime.Heap.Object (
    Object(..)
) where

data Object = NULL | Object {
    name :: String
} deriving (Show,Eq)