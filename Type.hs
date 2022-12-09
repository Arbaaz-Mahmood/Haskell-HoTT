{-# LANGUAGE TypeFamilies #-}

module Type where

import qualified Prelude
import Prelude hiding (id)
import qualified Control.Category as Cat
import Control.Category ((.), id)
import Data.Type.Equality ((:~:))
import qualified Data.Type.Equality as Eq
import qualified Data.Functor.Identity as Id
import qualified Data.Functor.Compose as Comp
import qualified Data.Functor.Product as Prod
import qualified Data.Functor.Sum as Sum
import qualified Data.Monoid as Mon
import qualified Control.Arrow as Arrow

-- The Type type represents a basic type in the type system.
data Type a = Type a

-- The IsType typeclass represents an instance of the Type type that satisfies the
-- necessary conditions to be considered a valid type.
class Cat.Category k => IsType k where
    type Type_ k :: * -> *
    toType :: Type_ k a -> k a a
    fromType :: k a a -> Type_ k a

instance IsType Type where
    type Type_ Type = Type
    toType = id
    fromType = id

instance (IsType k1, IsType k2) => IsType (Comp.Compose k1 k2) where
    type Type_ (Comp.Compose k1 k2) = Comp.Compose (Type_ k1) (Type_ k2)
    toType = Comp.Compose . Comp.Compose . toType . toType
    fromType = fromType . fromType . Comp.getCompose
module Type where

-- Basic types
data Bool   = False | True
data Nat    = Zero | Succ Nat
data List a = Nil | Cons a (List a)
data Pair a b = Pair a b

-- Basic type classes
class Eq a where
  (==) :: a -> a -> Bool

class Ord a where
  (<=) :: a -> a -> Bool

class Show a where
  show :: a -> String

-- Basic type class instances
instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False

instance Ord Bool where
  False <= _    = True
  True  <= True = True
  _     <= _    = False

instance Show Bool where
  show False = "False"
  show True  = "True"

instance Eq Nat where
  Zero   == Zero   = True
  Succ x == Succ y = x == y
  _      == _      = False

instance Ord Nat where
  Zero   <= _       = True
  Succ x <= Succ y  = x <= y
  _      <= _       = False

instance Show Nat where
  show Zero   = "Zero"
  show (Succ x) = "Succ (" ++ show x ++ ")"

instance Eq a => Eq (List a) where
  Nil     == Nil     = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _       == _       = False

instance Ord a => Ord (List a) where
  Nil     <= _       = True
  Cons x xs <= Cons y ys = x <= y && xs <= ys
  _       <= _       = False

instance Show a => Show (List a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

instance (Eq a
