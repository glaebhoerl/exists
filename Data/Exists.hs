{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Existential datatypes holding evidence of constraints and type classes for existential datatypes.
module Data.Exists (module Data.Exists.Internal) where

import Data.Exists.Internal
import Data.Exists.Defaults

import Prelude                         ((.), error)
import Unsafe.Coerce                   (unsafeCoerce)
import qualified Data.Traversable as T (foldMapDefault, fmapDefault)
import Data.Dynamic                    (toDyn, fromDyn)
import Control.Comonad                 (liftW)
import Control.Constraint.Combine      (Empty)
import Data.Typeable                   (Typeable)
import Control.Exception               (Exception)

import Data.Dynamic                    (Dynamic)
import GHC.Exts                        (Any)
import Data.Anything                   (Anything      (..),
                                        Anything1     (..))
import Control.Exception               (SomeException (..))

import Prelude                         (Show          (..),
                                        Functor       (..))
import Data.Foldable                   (Foldable      (..))
import Data.Traversable                (Traversable   (..))
import Data.Functor.Contravariant      (Contravariant (..))
import Data.Functor.Extend             (Extend        (..))
import Control.Comonad                 (Comonad       (..))
import Control.Comonad.Env.Class       (ComonadEnv    (..))
import Control.Comonad.Traced.Class    (ComonadTraced (..))
import Control.Comonad.Store.Class     (ComonadStore  (..))
import Data.Copointed                  (Copointed     (..))


-- | @'ConstraintOf' 'Any' = 'Empty'@
instance Existential Any where
    type ConstraintOf Any = Empty
    exists = unsafeCoerce
    apply f a = f a
    -- this is OK, because f by its type signature must be completely parametric
    -- with respect to a

-- | @'ConstraintOf1' 'Any' = 'Empty'@
instance Existential1 Any where
    type ConstraintOf1 Any = Empty
    exists1 = unsafeCoerce
    apply1 f a = f a
    -- likewise

-- | @'ConstraintOf' 'Anything' = 'Empty'@
instance Existential Anything where
    type ConstraintOf Anything = Empty
    exists = Anything
    apply f (Anything a) = f a

-- | @'ConstraintOf1' 'Anything1' = 'Empty'@
instance Existential1 Anything1 where
    type ConstraintOf1 Anything1 = Empty
    exists1 = Anything1
    apply1 f (Anything1 a) = f a

-- | @'ConstraintOf' 'Dynamic' = 'Typeable'@
instance Existential Dynamic where
    type ConstraintOf Dynamic = Typeable
    exists = toDyn
    apply f d = f (fromDyn (error "this can't be happening!") d)
    -- if I'm thinking correctly, nothing bad can result from this, because:
    -- - f can only use what Typeable provides;
    -- - typeOf is required to work on bottom values;
    -- - if f tries to cast its argument to the type which was in the Dynamic,
    --   the argument will be the value from the Dynamic and won't be bottom;
    -- - if f tries to cast its argument to a different type, the argument will
    --   be bottom, but Typeable won't allow the cast to succeed and it won't
    --   matter.

-- | @'ConstraintOf' 'SomeException' = 'Exception'@
instance Existential SomeException where
    type ConstraintOf SomeException = Exception
    exists = SomeException
    apply f (SomeException e) = f e

-- instance Show (Exists Exception) where
--    show = apply show
-- instance Exception (Exists Exception) where
--    fromException = fromExceptionDefault
--    toException   = toExceptionDefault
--
-- this unfortunately can't work, because Exception requires Typeable as a
-- superclass, Typeable only has typeOf as its method, and Typeable.cast will
-- indiscriminately unsafeCoerce based on the result of typeOf: so even if we
-- pass through the typeOf the underlying type, we're screwed because
-- Exists Exception is not physically that type. If we were to use the typeOf
-- (Exists Exception), we'd still be screwed from the other direction, but we
-- can't do that because Typeable isn't available for Constraints.

instance            Show (Exists  Show)              where
    show      = showDefault
    showsPrec = showsPrecDefault

instance         Functor (Exists1 Functor)           where
    fmap      = fmapDefault

instance        Foldable (Exists1 Foldable)          where
    fold      = foldDefault
    foldMap   = foldMapDefault
    foldl     = foldlDefault
    foldr     = foldrDefault
    foldl1    = foldl1Default
    foldr1    = foldr1Default

instance         Functor (Exists1 Traversable)       where
    fmap      = T.fmapDefault

instance        Foldable (Exists1 Traversable)       where
    foldMap   = T.foldMapDefault

instance     Traversable (Exists1 Traversable)       where
    traverse  = traverseDefault
    sequenceA = sequenceADefault
    mapM      = mapMDefault
    sequence  = sequenceDefault

instance   Contravariant (Exists1 Contravariant)     where
    contramap = contramapDefault

instance         Functor (Exists1 Extend)            where
    fmap f    = apply1 (exists1 . fmap f)

instance          Extend (Exists1 Extend)            where
    duplicate = duplicateDefault

instance         Functor (Exists1 Comonad)           where
    fmap      = liftW

instance          Extend (Exists1 Comonad)           where
    duplicate = apply1 (exists1 . fmap exists1 . duplicate)

instance         Comonad (Exists1 Comonad)           where
    extract   = extractDefault

instance         Functor (Exists1 (ComonadEnv e))    where
    fmap      = liftW

instance          Extend (Exists1 (ComonadEnv e))    where
    duplicate = apply1 (exists1 . fmap exists1 . duplicate)

instance         Comonad (Exists1 (ComonadEnv e))    where
    extract   = apply1 extract

instance    ComonadEnv e (Exists1 (ComonadEnv e))    where
    ask       = askDefault

instance         Functor (Exists1 (ComonadTraced m)) where
    fmap      = liftW

instance          Extend (Exists1 (ComonadTraced m)) where
    duplicate = apply1 (exists1 . fmap exists1 . duplicate)

instance         Comonad (Exists1 (ComonadTraced m)) where
    extract   = apply1 extract

instance ComonadTraced m (Exists1 (ComonadTraced m)) where
    trace     = traceDefault

instance         Functor (Exists1 (ComonadStore s))  where
    fmap      = liftW

instance          Extend (Exists1 (ComonadStore s))  where
    duplicate = apply1 (exists1 . fmap exists1 . duplicate)

instance         Comonad (Exists1 (ComonadStore s))  where
    extract   = apply1 extract

instance  ComonadStore s (Exists1 (ComonadStore s))  where
    pos       = posDefault
    peek      = peekDefault
    peeks     = peeksDefault
    seek      = seekDefault
    seeks     = seeksDefault

instance       Copointed (Exists1 Copointed)         where
    copoint   = copointDefault
