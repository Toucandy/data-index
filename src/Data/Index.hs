{-# LANGUAGE Rank2Types #-}

-- | 'Index', necessary instances for it, 'end' and 'mid' for any container
module Data.Index where

import Control.Applicative
import Control.Monad

-- | This type can contain any container-independent values to be indexed with
newtype Index t i = Index {runIndex :: forall a . t a -> i}

-- | Flipped version of 'runIndex'
run :: t a -> Index t i -> i
run = flip runIndex

-- | Reader-like Monad instance
instance Monad (Index t) where
    idx >>= f = Index $ \t -> run t $ f $ run t idx

-- | Reader-like Applicative instance
instance Applicative (Index t) where
    pure i = Index (const i)
    (<*>) = ap

-- | Reader-like Functor instance
instance Functor (Index t) where
    fmap = ap . return

-- | Num instance is done via 'liftA2' and 'pure'
instance Num i => Num (Index t i) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger

-- | The end of any linear container
end :: Foldable t => Index t Int
end = Index (\t -> length t - 1)

-- | The middle of any linear container
mid :: Foldable t => Index t Int
mid = Index (\t -> length t `div` 2)
