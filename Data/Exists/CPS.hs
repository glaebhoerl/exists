{-# LANGUAGE ConstraintKinds, TypeFamilies, RankNTypes #-}

-- | CPS-transformed versions of @Exists@ and @Exists1@, provided for completeness and curiosity.

module Data.Exists.CPS(Exists(..), Exists1(..)) where

import Data.Exists.Internal(Existential(..), Existential1(..))

newtype Exists c = Exists { withExists :: forall r. (forall a. c a => a -> r) -> r }

-- | @'ConstraintOf' ('Exists' c) = c@
instance Existential (Exists c) where
    type ConstraintOf (Exists c) = c
    exists  a = Exists $ \f -> f a
    apply f e = withExists e f

newtype Exists1 c a = Exists1 { withExists1 :: forall r. (forall f. c f => f a -> r) -> r }

-- | @'ConstraintOf1' ('Exists1' c) = c@
instance Existential1 (Exists1 c) where
    type ConstraintOf1 (Exists1 c) = c
    exists1  a = Exists1 $ \f -> f a
    apply1 f e = withExists1 e f
