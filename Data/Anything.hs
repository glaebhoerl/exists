{-# LANGUAGE GADTs, DeriveDataTypeable #-}

-- | Useless existential datatypes holding evidence of no constraint.
module Data.Anything where

import Data.Typeable

-- | A datatype containing anything. You can't do anything with it.
data Anything where
     Anything :: a -> Anything
     deriving Typeable

-- | A datatype containing any @* -> *@ type constructor applied to 'a'. You can't do anything with it.
data Anything1 a where
     Anything1 :: f a -> Anything1 a
     deriving Typeable

instance Show Anything where
    show = const "Anything"

instance Show (Anything1 a) where
    show = const "Anything1"
