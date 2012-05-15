{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds, FlexibleContexts  #-}

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
{-
import Data.Functor.Extend            (Extend        (..))
import Control.Comonad                (Comonad       (..))
import Control.Comonad.Env.Class      (ComonadEnv    (..))
import Control.Comonad.Traced.Class   (ComonadTraced (..))
import Control.Comonad.Store.Class    (ComonadStore  (..))
import Data.Copointed                 (Copointed     (..))
-}

-- * Prelude.Show
showDefault      :: ExistentialWith Show e                          => e -> String
showDefault            = apply show

showsPrecDefault :: ExistentialWith Show e                          => Int -> e -> ShowS
showsPrecDefault n e s = apply (\a -> showsPrec n a s) e

-- * Prelude.Functor
fmapDefault      :: ExistentialWith1 Functor e                      => (a -> b) -> e a -> e b
fmapDefault f          = apply1 (exists1 . fmap f)

-- * Data.Foldable.Foldable
foldDefault      :: (ExistentialWith1 Foldable e, Monoid m)         => e m -> m
foldDefault            = apply1 fold

foldMapDefault   :: (ExistentialWith1 Foldable e, Monoid m)         => (a -> m) -> e a -> m
foldMapDefault f       = apply1 (foldMap f)

foldrDefault     :: ExistentialWith1 Foldable e                     => (a -> b -> b) -> b -> e a -> b
foldrDefault f x       = apply1 (foldr f x)

foldlDefault     :: ExistentialWith1 Foldable e                     => (a -> b -> a) -> a -> e b -> a
foldlDefault f x       = apply1 (foldl f x)

foldr1Default    :: ExistentialWith1 Foldable e                     => (a -> a -> a) -> e a -> a
foldr1Default f        = apply1 (foldr1 f)

foldl1Default    :: ExistentialWith1 Foldable e                     => (a -> a -> a) -> e a -> a
foldl1Default f        = apply1 (foldl1 f)

-- * Data.Traversable.Traversable
traverseDefault  :: (ExistentialWith1 Traversable e, Applicative f) => (a -> f b) -> e a -> f (e b)
traverseDefault f      = apply1 (fmap exists1 . traverse f)

sequenceADefault :: (ExistentialWith1 Traversable e, Applicative f) => e (f a) -> f (e a)
sequenceADefault       = apply1 (fmap exists1 . sequenceA)

mapMDefault      :: (ExistentialWith1 Traversable e, Monad m)       => (a -> m b) -> e a -> m (e b)
mapMDefault f          = apply1 (liftM exists1 . mapM f)

sequenceDefault  :: (ExistentialWith1 Traversable e, Monad m)       => e (m a) -> m (e a)
sequenceDefault        = apply1 (liftM exists1 . sequence)

-- * Data.Functor.Contravariant.Contravariant
contramapDefault :: ExistentialWith1 Contravariant e                => (a -> b) -> e b -> e a
contramapDefault f     = apply1 (exists1 . contramap f)

{-
-- * Data.Functor.Extend.Extend
duplicateDefault :: ExistentialWith1 Extend e                       => e a -> e (e a)
duplicateDefault       = apply1 (exists1 . fmap exists1 . duplicate)

-- extendDefault :: ExistentialWith1 Extend e                       => (e a -> b) -> e a -> e b
-- extendDefault       = _

-- * Control.Comonad.Comonad
extractDefault   :: ExistentialWith1 Comonad e                      => e a -> a
extractDefault         = apply1 extract

-- * Control.Comonad.Env.Class.ComonadEnv
askDefault       :: ExistentialWith1 (ComonadEnv env) e             => e a -> env
askDefault             = apply1 ask

-- * Control.Comonad.Traced.Class.ComonadTraced
traceDefault     :: ExistentialWith1 (ComonadTraced m) e            => m -> e a -> a
traceDefault a         = apply1 (trace a)

-- * Control.Comonad.Store.Class.ComonadStore
posDefault       :: ExistentialWith1 (ComonadStore s) e             => e a -> s
posDefault             = apply1 pos

peekDefault      :: ExistentialWith1 (ComonadStore s) e             => s -> e a -> a
peekDefault a          = apply1 (peek a)

peeksDefault     :: ExistentialWith1 (ComonadStore s) e             => (s -> s) -> e a -> a
peeksDefault f         = apply1 (peeks f)

seekDefault      :: ExistentialWith1 (ComonadStore s) e             => s -> e a -> e a
seekDefault a          = apply1 (exists1 . seek a)

seeksDefault     :: ExistentialWith1 (ComonadStore s) e             => (s -> s) -> e a -> e a
seeksDefault f         = apply1 (exists1 . seeks f)

-- * Data.Copointed.Copointed
copointDefault   :: ExistentialWith1 Copointed e                    => e a -> a
copointDefault         = apply1 copoint
-}

{-# INLINE showDefault      #-}
{-# INLINE showsPrecDefault #-}
{-# INLINE fmapDefault      #-}
{-# INLINE foldDefault      #-}
{-# INLINE foldMapDefault   #-}
{-# INLINE foldrDefault     #-}
{-# INLINE foldlDefault     #-}
{-# INLINE foldr1Default    #-}
{-# INLINE foldl1Default    #-}
{-# INLINE traverseDefault  #-}
{-# INLINE sequenceADefault #-}
{-# INLINE mapMDefault      #-}
{-# INLINE sequenceDefault  #-}
{-# INLINE contramapDefault #-}
{-  INLINE duplicateDefault  -}
{-  INLINE extendDefault     -}
{-  INLINE extractDefault    -}
{-  INLINE askDefault        -}
{-  INLINE traceDefault      -}
{-  INLINE posDefault        -}
{-  INLINE peekDefault       -}
{-  INLINE peeksDefault      -}
{-  INLINE seekDefault       -}
{-  INLINE seeksDefault      -}
{-  INLINE copointDefault    -}
