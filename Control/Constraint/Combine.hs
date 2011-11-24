{-# LANGUAGE PolyKinds, ConstraintKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

-- | A type-level combinator for combining constraint constructors.
--   This is useful because you can't inherently write an 'Data.Exists.Exists' type or 'Data.Exists.Existential' instance referencing more than one.
module Control.Constraint.Combine where

-- | Combines two constraint constructors of kind X -> 'GHC.Prim.Constraint', where X is any kind.
--   This is similar in spirit to
--   > type (c :&: d) a = (c a, d a)
--   except that it can be partially applied.
--
--   > f :: ((Eq :&: Enum :&: Bounded) a) => a -> Bool
--   is equivalent to
--   > f :: (Eq a, Enum a, Bounded a) => a -> Bool
--
--   > g :: ((Applicative :&: Monad) m) => a -> b -> m (a, b)
--   is equivalent to
--   > g :: (Applicative m, Monad m) => a -> b -> m (a, b)
class    (c a, d a) => (c :&: d) a
instance (c a, d a) => (c :&: d) a
infixl 7 :&:

-- | The same as ':&:', for those who prefer English.
type c `And` d = c :&: d
infixl 7 'And`

-- | An empty constraint, which implies nothing.
-- | ':&:' and 'Empty' form a type-level monoid with 'Empty' as the identity element.
class    Empty a
instance Empty a
