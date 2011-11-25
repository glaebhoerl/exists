{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Canonical existential datatypes holding evidence of constraints, and type classes for existential datatypes.
module Data.Exists (module Data.Exists.Internal) where

import Control.Constraint.Combine
import Data.Anything
import Data.Exists.Defaults as D
import Data.Exists.Internal

import Control.Exception
import Data.Dynamic
import Data.Foldable
import Data.Traversable     as T
import GHC.Prim (Any)
import Unsafe.Coerce

-- | 'ConstraintOf' 'Any' = 'Empty'
instance Existential Any where
    type ConstraintOf Any = Empty
    exists = unsafeCoerce
    apply f a = f a
    -- this is OK, because f by its type signature must be completely parametric
    -- with respect to a

-- | 'ConstraintOf1' 'Any' = 'Empty'
instance Existential1 Any where
    type ConstraintOf1 Any = Empty
    exists1 = unsafeCoerce
    apply1 f a = f a
    -- likewise

-- | 'ConstraintOf' 'Anything' = 'Empty'
instance Existential Anything where
    type ConstraintOf Anything = Empty
    exists = Anything
    apply f (Anything a) = f a

-- | 'ConstraintOf1' 'Anything1' = 'Empty'
instance Existential1 Anything1 where
    type ConstraintOf1 Anything1 = Empty
    exists1 = Anything1
    apply1 f (Anything1 a) = f a

-- | 'ConstraintOf' 'Dynamic' = 'Typeable'
instance Existential Dynamic where
    type ConstraintOf Dynamic = Typeable
    exists = toDyn
    apply f d = f (fromDyn (error "this can't be happening!") d)
    -- if I'm thinking straight, nothing bad can result from this:
    -- - the only things f is allowed use are what Typeable provides;
    -- - typeOf is required to work on bottom values;
    -- - if f tries to cast its argument to the type which was in the Dynamic,
    --   the argument will be the value from the Dynamic and won't be bottom;
    -- - if f tries to cast its argument to a different type, the argument will
    --   be bottom, but Typeable won't allow the cast to succeed and it won't
    --   matter.

-- | 'ConstraintOf' 'SomeException' = 'Exception'
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

instance        Show (Exists  Show)        where
    show      = showDefault
    showsPrec = showsPrecDefault

instance     Functor (Exists1 Functor)     where
    fmap      = D.fmapDefault

instance    Foldable (Exists1 Foldable)    where
    fold      = foldDefault
    foldMap   = D.foldMapDefault
    foldl     = foldlDefault
    foldr     = foldrDefault
    foldl1    = foldl1Default
    foldr1    = foldr1Default

instance     Functor (Exists1 Traversable) where
    fmap      = T.fmapDefault

instance    Foldable (Exists1 Traversable) where
    foldMap   = T.foldMapDefault

instance Traversable (Exists1 Traversable) where
    traverse  = traverseDefault
    sequenceA = sequenceADefault
    mapM      = mapMDefault
    sequence  = sequenceDefault
