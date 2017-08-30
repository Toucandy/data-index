{-# LANGUAGE Rank2Types #-}

module Index where

import Prelude hiding ((!!), take, drop, splitAt)
import qualified Prelude as P ((!!), take, drop, splitAt)
import Control.Applicative

-- Index type and its instances
data Index i = Index (forall a . [a] -> i)

instance Functor Index where
    fmap f (Index i) = Index (f . i)

instance Applicative Index where
    pure i = Index (const i)
    Index f <*> Index i = Index (\as -> (f as) (i as))

instance Num i => Num (Index i) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

-- Function replacement for Data.List
(!!) :: [a] -> Index Int -> a
xs !! Index i = xs P.!! i xs

take :: Index Int -> [a] -> [a]
take (Index i) xs = P.take (i xs) xs

drop :: Index Int -> [a] -> [a]
drop (Index i) xs = P.drop (i xs) xs

splitAt :: Index Int -> [a] -> ([a], [a])
splitAt (Index i) xs = P.splitAt (i xs) xs

-- Using
end = Index (\a -> length a - 1)
mid = Index (\a -> length a `div` 2)
