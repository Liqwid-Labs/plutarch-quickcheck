{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.QuickCheck.Function.Internal (
    PFunction (..),
) where

import Data.Kind (Type, Constraint)
import Plutarch (S, (:-->))
import Plutarch.Test.QuickCheck.Instances (TestableTerm)

class PFunction (pf :: (S -> Type) -> (S -> Type) -> Type) where
    type PFunctionConstraint pf (a :: S -> Type) (b :: S -> Type) :: Constraint
    type PFunctionConstraint pf a b = ()
    applyPFun :: forall (a :: S -> Type) (b :: S -> Type). pf a b -> TestableTerm (a :--> b)
