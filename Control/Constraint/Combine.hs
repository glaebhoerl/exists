{-# LANGUAGE PolyKinds, ConstraintKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This module contains a type-level combinator for combining constraint constructors.
--
--   This is useful because you can't otherwise write an @'Exists'@ type or @'Existential'@ instance referencing more than one at the same time.
module Control.Constraint.Combine where

import Data.Exists.Internal -- just for haddock

-- | Combine two constraint constructors of kind @&#967; -> 'Constraint'@, where @&#967;@ is any kind.
--
--   This is the same as
--
--   > type (c :&: d) a = (c a, d a)
--
--   except that it can be partially applied.
--
--   > f :: ((Eq :&: Enum :&: Bounded) a) => a -> Bool
--
--   is equivalent to
--
--   > f :: (Eq a, Enum a, Bounded a) => a -> Bool
class    (c a, d a) => (c :&: d) a
instance (c a, d a) => (c :&: d) a
infixl 7 :&:

-- | The same as @':&:'@.
type c `And` d = c :&: d
infixl 7 `And`

-- | An empty constraint which implies nothing.
--
--  @':&:'@ and @'Empty'@ form a type-level monoid with @'Empty'@ as the identity element.
class    Empty a
instance Empty a
