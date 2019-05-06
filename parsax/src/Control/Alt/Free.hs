{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Alternative.Free
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, Rank2Types
--
-- Left distributive 'Alternative' functors for free, based on a design
-- by Stijn van Drongelen.
----------------------------------------------------------------------------

module Control.Alt.Free
  ( Alt(..)
  , AltF(..)
  , runAlt
  ) where

import           Data.Functor.Alt ((<!>))
import qualified Data.Functor.Alt as Alt
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

infixl 3 `Ap`

data AltF f a where
  Ap     :: f a -> Alt f (a -> b) -> AltF f b
  Pure   :: a                     -> AltF f a

newtype Alt f a = Alt { alternatives :: NonEmpty (AltF f a) }

instance Functor (AltF f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap x g) = x `Ap` fmap (f .) g

instance Functor (Alt f) where
  fmap f (Alt xs) = Alt $ fmap (fmap f) xs

instance Applicative (Alt f) where
  pure a = Alt (pure (Pure a))
  {-# INLINE pure #-}

  (Alt xs) <*> ys = Alt (xs >>= alternatives . (`ap'` ys))
    where
      ap' :: AltF f (a -> b) -> Alt f a -> Alt f b

      Pure f `ap'` u      = fmap f u
      (u `Ap` f) `ap'` v  = Alt (pure (u `Ap` (flip <$> f) <*> v))
  {-# INLINE (<*>) #-}

instance Alt.Alt (Alt f) where
  Alt as <!> Alt bs = Alt (as <> bs)
  {-# INLINE (<!>) #-}

-- | Given a natural transformation from @f@ to @g@, this gives a canonical monoidal natural transformation from @'Alt' f@ to @g@.
runAlt :: forall f g a. (Alt.Alt g, Applicative g) => (forall x. f x -> g x) -> Alt f a -> g a
runAlt u xs0 = go xs0 where

  go  :: Alt f b -> g b
  go (Alt xs) = foldr (\ r a -> (go2 r) <!> a) (go2 (NE.last xs)) (NE.init xs)

  go2 :: AltF f b -> g b
  go2 (Pure a) = pure a
  go2 (Ap x f) = flip id <$> u x <*> go f
{-# INLINABLE runAlt #-}
