{-# LANGUAGE ApplicativeDo #-}

-- | Redefined index-related functions for Data.Sequence
module Data.Sequence.Index (
    module Data.Index,
    module Data.Sequence.Index
    ) where

import Data.Index
import Prelude hiding (take, drop, splitAt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- |
-- >>> index (Seq.fromList [0..100]) 100
-- 100
-- >>> index (Seq.fromList [0..100]) end
-- 100
-- >>> index (Seq.fromList [0..100]) (end-4)
-- 96
-- >>> index (Seq.fromList [0..100]) 51
-- 51
-- >>> index (Seq.fromList [0..100]) (mid+1)
-- 51
index :: Seq a -> Index Seq Int -> a
index t idx = run t $ do
    i <- idx
    return $ Seq.index t i

-- |
-- >>> adjust (pred . pred) 25 (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmnopqrstuvwxyx"
-- >>> adjust (pred . pred) end (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmnopqrstuvwxyx"
-- >>> adjust (pred . pred) mid (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmlopqrstuvwxyz"
-- >>> adjust (pred . pred) (mid+1) (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmnmpqrstuvwxyz"
adjust :: (a -> a) -> Index Seq Int -> Seq a -> Seq a
adjust f idx t = run t $ do
    i <- idx
    return $ Seq.adjust f i t

-- |
-- >>> update mid '?' (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklm?opqrstuvwxyz"
-- >>> update 24 '?' (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmnopqrstuvwx?z"
-- >>> update (end-1) '?' (Seq.fromList ['a'..'z'])
-- fromList "abcdefghijklmnopqrstuvwx?z"
update :: Index Seq Int -> a -> Seq a -> Seq a
update idx x t = run t $ do
    i <- idx
    return $ Seq.update i x t

-- |
-- >>> take 10 (Seq.fromList ['a'..'z'])
-- fromList "abcdefghij"
-- >>> take (mid - 3) (Seq.fromList ['a'..'z'])
-- fromList "abcdefghij"
take :: Index Seq Int -> Seq a -> Seq a
take idx t = run t $ do
    i <- idx
    return $ Seq.take i t

-- |
-- >>> drop 13 (Seq.fromList ['a'..'z'])
-- fromList "nopqrstuvwxyz"
-- >>> drop mid (Seq.fromList ['a'..'z'])
-- fromList "nopqrstuvwxyz"
drop :: Index Seq Int -> Seq a -> Seq a
drop idx t = run t $ do
    i <- idx
    return $ Seq.drop i t

-- |
-- >>> splitAt mid (Seq.fromList "Hello World!")
-- (fromList "Hello ",fromList "World!")
-- >>> splitAt (mid - 1) (Seq.fromList "Hello World!")
-- (fromList "Hello",fromList " World!")
splitAt :: Index Seq Int -> Seq a -> (Seq a, Seq a)
splitAt idx t = run t $ do
    i <- idx
    return $ Seq.splitAt i t
