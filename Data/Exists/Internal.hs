{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, ConstraintKinds #-}

module Data.Exists.Internal (Constraint, module Data.Exists.Internal) where

import GHC.Prim (Constraint)

-- | A datatype which holds a value of a type satisfying the constraint 'c', hiding the type, and evidence for the constraint, so that it can be taken advantage of by pattern matching later.
--
--   An example:
--
--   > foo :: Exists Show
--   > foo = Exists (Just 9 :: Maybe Int)
--   >
--   > printExists :: Exists Show -> IO ()
--   > printExists (Exists e) = print e
--   >
--   > main = printExists foo -- prints "Just 9"
data Exists c where
     Exists :: c a => a -> Exists c

-- | A type class for existential datatypes.
class Existential e where
    type ConstraintOf e :: * -> Constraint
    -- | Construct 'e' from a value of a type satisfying the constraint.
    exists :: (ConstraintOf e) a => a -> e
    -- | Apply a function requiring the constraint to the held value.
    apply  :: (forall a. (ConstraintOf e) a => a -> r) -> e -> r

-- | 'ConstraintOf' ('Exists' c) = c
instance Existential (Exists c) where
    type ConstraintOf (Exists c) = c
    exists = Exists
    apply f (Exists a) = f a

-- | Translate between different existential datatypes holding evidence for the same constraint.
translate :: (Existential e1, Existential e2, ConstraintOf e1 ~ ConstraintOf e2) => e1 -> e2
translate = apply exists

-- | A * -> * kinded version of 'Exists', which holds a value of a type constructor applied to a type, hiding the type constructor, and holding evidence for a constraint on the type constructor.
data Exists1 c a where
     Exists1 :: c f => f a -> Exists1 c a

-- | A version of 'Existential' for * -> *.
class Existential1 e where
    type ConstraintOf1 e :: (* -> *) -> Constraint
    -- | Construct 'e' from a value of a type constructor applied to a type, where the type constructor satisfies the constraint.
    exists1 :: (ConstraintOf1 e) f => f a -> e a
    -- | Apply a function requiring the constraint to the held value.
    apply1  :: (forall f. (ConstraintOf1 e) f => f a -> r) -> e a -> r

-- | 'ConstraintOf1' ('Exists1' c) = c
instance Existential1 (Exists1 c) where
    type ConstraintOf1 (Exists1 c) = c
    exists1 = Exists1
    apply1 f (Exists1 a) = f a

-- | Translate between different existential datatypes holding evidence for the same constraint on a * -> * type constructor.
translate1 :: (Existential1 e1, Existential1 e2, ConstraintOf1 e1 ~ ConstraintOf1 e2) => e1 a -> e2 a
translate1 = apply1 exists1
