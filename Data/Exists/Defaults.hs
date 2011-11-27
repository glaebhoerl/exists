{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds  #-}

-- | Default functions which can be used as method implementations when writing type class instances for existential datatypes.
module Data.Exists.Defaults where

import Data.Exists.Internal

import Prelude                        (String, Int, ShowS, Monad, (.))
import Control.Applicative            (Applicative)
import Control.Monad                  (liftM)
import Data.Monoid                    (Monoid)

import Prelude                        (Show          (..),
                                       Functor       (..))
import Data.Foldable                  (Foldable      (..))
import Data.Traversable               (Traversable   (..))
import Data.Functor.Contravariant     (Contravariant (..))
import Data.Functor.Extend            (Extend        (..))
import Control.Comonad                (Comonad       (..))
import Control.Comonad.Env.Class      (ComonadEnv    (..))
import Control.Comonad.Traced.Class   (ComonadTraced (..))
import Control.Comonad.Store.Class    (ComonadStore  (..))
import Data.Copointed                 (Copointed     (..))

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

-- * Data.Functor.Contravariant.Contravariant
contramapDefault :: (Existential1 e, ConstraintOf1 e ~ Contravariant) => (a -> b) -> e b -> e a
contramapDefault f = apply1 (exists1 . contramap f)

-- * Data.Functor.Extend.Extend
duplicateDefault :: (Existential1 e, ConstraintOf1 e ~ Extend) => e a -> e (e a)
duplicateDefault = apply1 (exists1 . fmap exists1 . duplicate)

-- extendDefault :: (Existential1 e, ConstraintOf1 e ~ Extend) => (e a -> b) -> e a -> e b

-- * Control.Comonad.Comonad
extractDefault :: (Existential1 e, ConstraintOf1 e ~ Comonad) => e a -> a
extractDefault = apply1 extract

-- * Control.Comonad.Env.Class.ComonadEnv
askDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadEnv env) => e a -> env
askDefault = apply1 ask

-- * Control.Comonad.Traced.Class.ComonadTraced
traceDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadTraced m) => m -> e a -> a
traceDefault a = apply1 (trace a)

-- * Control.Comonad.Store.Class.ComonadStore
posDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadStore s) => e a -> s
posDefault = apply1 pos

peekDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadStore s) => s -> e a -> a
peekDefault a = apply1 (peek a)

peeksDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadStore s) => (s -> s) -> e a -> a
peeksDefault f = apply1 (peeks f)

seekDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadStore s) => s -> e a -> e a
seekDefault a = apply1 (exists1 . seek a)

seeksDefault :: (Existential1 e, ConstraintOf1 e ~ ComonadStore s) => (s -> s) -> e a -> e a
seeksDefault f = apply1 (exists1 . seeks f)

-- * Data.Copointed.Copointed
copointDefault :: (Existential1 e, ConstraintOf1 e ~ Copointed) => e a -> a
copointDefault = apply1 copoint
