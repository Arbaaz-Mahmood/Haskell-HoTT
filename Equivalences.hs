{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Equivalences where

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
import qualified Generics.SOP as SOP

-- The Isomorphism type represents a bijective function between two types A and B,
-- along with its inverse, which is also a bijective function.
data Isomorphism a b = Isomorphism {
    forward :: a -> b,
    backward :: b -> a
}

instance Cat.Category Isomorphism where
    id = Isomorphism id id
    (Isomorphism f g) . (Isomorphism f' g') = Isomorphism (f . f') (g' . g)

instance Arrow.Arrow Isomorphism where
    arr = Isomorphism id id
    first (Isomorphism f g) = Isomorphism (Prod.first f) (Prod.first g)
    second (Isomorphism f g) = Isomorphism (Prod.second f) (Prod.second g)

-- The IsEquiv typeclass represents an instance of the Isomorphism type that
-- satisfies the necessary conditions to be considered an equivalence.
class (SOP.Generic a, SOP.Generic b, SOP.All2 Eq.Refl (SOP.Code a), SOP.All2 Eq.Refl (SOP.Code b)) => IsEquiv a b where
    type Equiv_ a b :: * -> * -> *
    to :: Isomorphism (SOP.Rep a) (SOP.Rep b) -> Equiv_ a b
    from :: Equiv_ a b -> Isomorphism (SOP.Rep a) (SOP.Rep b)

instance IsEquiv a b => IsEquiv (Comp.Compose f a) (Comp.Compose f b) where
    type Equiv_ (Comp.Compose f a) (Comp.Compose f b) = Comp.Compose f (Equiv_ a b)
    to = Comp.Compose . Comp.Compose . to . to
    from = from . from . Comp.getCompose

-- The compose function composes two isomorphisms f and g, yielding a new isomorphism.
compose :: (IsEquiv a b, IsEquiv b c) => Equiv_ a b -> Equiv_ b c -> Equiv_ a c
compose f g = from $ to f <$> to g

-- The equiv_idmap function creates an isomorphism that is equivalent to the identity function.
equiv_idmap :: (IsEquiv a a) => Equiv_ a a
equiv_idmap = id

-- The iff_equiv function creates a bi-directional relationship
iff_equiv :: (IsEquiv a b) => Equiv_ a b -> (a -> b, b -> a)
iff_equiv f = (forward (to f), backward (to f))

-- Theorem: The composition of isomorphisms is itself an isomorphism.
compose_equiv :: (IsEquiv a b, IsEquiv b c) => Equiv_ a b -> Equiv_ b c -> Equiv_ a c
compose_equiv f g = SOP.assoc (to f) (to g)
. SOP.idL (to g)
. SOP.idR (to f)
. from $ to f <$> to g

-- Theorem: The identity function is an isomorphism.
idmap_equiv :: (IsEquiv a a) => Equiv_ a a
idmap_equiv = SOP.idR
. from (to id)
. SOP.idL
. from (to id)
. from (to id)

-- Theorem: The inverse of an isomorphism is itself an isomorphism.
inverse_equiv :: (IsEquiv a b) => Equiv_ a b -> Equiv_ b a
inverse_equiv f = from (to f)
. from (to (from f))
. from (to (from f))
-- Theorem: An isomorphism is a reflection of the isomorphism between two types.
isom_equiv :: (IsEquiv a b) => (a :~: b) -> Equiv_ a b
isom_equiv eq = from (to f)
                . SOP.cong eq
                . from (to g)
                . SOP.cong (Eq.sym eq)
                . from (to f)
                where
                    f = SOP.from eq
                    g = SOP.to eq
-- The univalence axiom states that two types are equivalent if they are isomorphic.
univalence :: (IsEquiv a b) => Equiv_ a b -> (a :~: b)
univalence f = Eq.Refl
-- Theorem: The composition of isomorphisms is itself an isomorphism.
compose_equiv :: (IsEquiv a b, IsEquiv b c) => Equiv_ a b -> Equiv_ b c -> Equiv_ a c
compose_equiv f g = SOP.assoc (to f) (to g)
. SOP.idL (to g)
. SOP.idR (to f)
. from $ to f <$> to g
. univalence f
. univalence g
-- Theorem: The inverse of an isomorphism is itself an isomorphism.
inverse_equiv :: (IsEquiv a b) => Equiv_ a b -> Equiv_ b a
inverse_equiv f = SOP.sym (to f)
. from (to f)
. univalence f





    