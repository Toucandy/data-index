-- | Redefined index-related functions for Data.List
module Data.List.Index (
    module Data.Index,
    module Data.List.Index
    ) where

import Data.Index
import Prelude hiding ((!!), take, drop, splitAt)
import qualified Data.List as List

-- |
-- >>> [1..7] !! 1
-- 2
-- >>> [1..7] !! end
-- 7
-- >>> [1..7] !! (end-2)
-- 5
-- >>> [1..7] !! mid
-- 4
(!!) :: [a] -> Index [] Int -> a
t !! idx = run t $ do
    i <- idx
    return $ t List.!! i

-- |
-- >>> take 10 ['a'..'z']
-- "abcdefghij"
-- >>> take (mid - 3) ['a'..'z']
-- "abcdefghij"
take :: Index [] Int -> [a] -> [a]
take idx t = run t $ do
    i <- idx
    return $ List.take i t

-- |
-- >>> drop 13 ['a'..'z']
-- "nopqrstuvwxyz"
-- >>> drop mid ['a'..'z']
-- "nopqrstuvwxyz"
drop :: Index [] Int -> [a] -> [a]
drop idx t = run t $ do
    i <- idx
    return $ List.drop i t

-- |
-- >>> splitAt mid "Hello World!"
-- ("Hello ","World!")
-- >>> splitAt (mid - 1) "Hello World!"
-- ("Hello"," World!")
splitAt :: Index [] Int -> [a] -> ([a], [a])
splitAt idx t = run t $ do
    i <- idx
    return $ List.splitAt i t
