{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds  #-}

-- Default functions which can be used as method implementations when writing type class instances for existential datatypes.
module Data.Exists.Defaults where

import Data.Exists.Internal

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Data.Monoid (Monoid)
import Data.Foldable
import Data.Traversable

import Prelude hiding (foldl, foldl1, foldr, foldr1, mapM, sequence)

-- * Prelude.Show
showDefault :: (Existential e, ConstraintOf e ~ Show) => e -> String
showDefault = apply show
{-# INLINE showDefault #-}

showsPrecDefault :: (Existential e, ConstraintOf e ~ Show) => Int -> e -> ShowS
showsPrecDefault n e s = apply (\a -> showsPrec n a s) e
{-# INLINE showsPrecDefault #-}

-- * Prelude.Functor
fmapDefault :: (Existential1 e, ConstraintOf1 e ~ Functor) => (a -> b) -> e a -> e b
fmapDefault f = apply1 (exists1 . fmap f)
{-# INLINE fmapDefault #-}

-- * Data.Foldable.Foldable
foldDefault :: (Existential1 e, ConstraintOf1 e ~ Foldable, Monoid m) => e m -> m
foldDefault = apply1 fold
{-# INLINE foldDefault #-}

foldMapDefault :: (Existential1 e, ConstraintOf1 e ~ Foldable, Monoid m) => (a -> m) -> e a -> m
foldMapDefault f = apply1 (foldMap f)
{-# INLINE foldMapDefault #-}

foldrDefault :: (Existential1 e, ConstraintOf1 e ~ Foldable) => (a -> b -> b) -> b -> e a -> b
foldrDefault f x = apply1 (foldr f x)
{-# INLINE foldrDefault #-}

foldlDefault :: (Existential1 e, ConstraintOf1 e ~ Foldable) => (a -> b -> a) -> a -> e b -> a
foldlDefault f x = apply1 (foldl f x)
{-# INLINE foldlDefault #-}

foldr1Default :: (Existential1 e, ConstraintOf1 e ~ Foldable) => (a -> a -> a) -> e a -> a
foldr1Default f = apply1 (foldr1 f)
{-# INLINE foldr1Default #-}

foldl1Default :: (Existential1 e, ConstraintOf1 e ~ Foldable) => (a -> a -> a) -> e a -> a
foldl1Default f = apply1 (foldl1 f)
{-# INLINE foldl1Default #-}

-- * Data.Traversable.Traversable
traverseDefault :: (Existential1 e, ConstraintOf1 e ~ Traversable, Applicative f) => (a -> f b) -> e a -> f (e b)
traverseDefault f = apply1 (fmap exists1 . traverse f)
{-# INLINE traverseDefault #-}

sequenceADefault :: (Existential1 e, ConstraintOf1 e ~ Traversable, Applicative f) => e (f a) -> f (e a)
sequenceADefault = apply1 (fmap exists1 . sequenceA)
{-# INLINE sequenceADefault #-}

mapMDefault :: (Existential1 e, ConstraintOf1 e ~ Traversable, Monad m) => (a -> m b) -> e a -> m (e b)
mapMDefault f = apply1 (liftM exists1 . mapM f)
{-# INLINE mapMDefault #-}

sequenceDefault :: (Existential1 e, ConstraintOf1 e ~ Traversable, Monad m) => e (m a) -> m (e a)
sequenceDefault = apply1 (liftM exists1 . sequence)
{-# INLINE sequenceDefault #-}

