{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.QuickCheck.Function.Internal (
  PFunction (..),
) where

import Data.Kind (Constraint, Type)
import Plutarch (S, (:-->))
import Plutarch.Test.QuickCheck.Instances (TestableTerm)

-- Note: PFunction is not exposed to the users.
-- PFunction defines the Plutarch function generators. It requires
-- an associated constraints that is for fulfilling 'WrapPFun'
-- constraints and 'applyPFun' which gives Plutarch function.
class PFunction (pf :: (S -> Type) -> (S -> Type) -> Type) where
  type PFunctionConstraint pf (a :: S -> Type) (b :: S -> Type) :: Constraint
  type PFunctionConstraint pf a b = ()
  applyPFun :: forall (a :: S -> Type) (b :: S -> Type). pf a b -> TestableTerm (a :--> b)
