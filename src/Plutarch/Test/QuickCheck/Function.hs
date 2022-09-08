{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck.Function (
  WrapPFun(..),
  lifty,
  PFun (..),
  PFunLift (..),
  PFunction (..),
  pattern PFn,
  pfunTable,
  pfunFinite,
  pfunLiftTable,
  pfunLiftFinite,  
) where

import Plutarch.Test.QuickCheck.Instances (TestableTerm(unTestableTerm))
import Plutarch.Test.QuickCheck.Function.Internal (PFunction (..))
import Plutarch.Test.QuickCheck.Function.PFun (PFun(..), pfunTable, pfunFinite)
import Plutarch.Test.QuickCheck.Function.PFunLift (PFunLift(..), pfunLiftTable, pfunLiftFinite)

import Plutarch.Prelude (S, Type, Term, (:-->))
import Test.QuickCheck (
  Gen,
  Arbitrary (arbitrary, shrink),
 )

data WrapPFun (a :: S -> Type) (b :: S -> Type)
    = forall pf. (PFunction pf, Arbitrary (pf a b), Show (pf a b)) => WrapPFun (pf a b)

instance PFunction WrapPFun where
    applyPFun (WrapPFun pf) = applyPFun pf

instance
  forall (a :: S -> Type) (b :: S -> Type).
  PFunctionConstraint PFun a b =>
  Arbitrary (WrapPFun a b)
  where
  arbitrary = WrapPFun <$> (arbitrary :: Gen (PFun a b))
  shrink (WrapPFun x) = WrapPFun <$> shrink x

instance forall (a :: S -> Type) (b :: S -> Type). Show (WrapPFun a b) where
  show (WrapPFun x) = show x

lifty :: forall a b. (PFunctionConstraint PFunLift a b) => Gen (WrapPFun a b)
lifty = WrapPFun <$> (arbitrary :: Gen (PFunLift a b))

{-# COMPLETE PFn #-}
pattern PFn ::
  forall (a :: S -> Type) (b :: S -> Type) (pf :: (S -> Type) -> (S -> Type) -> Type).
  PFunction pf =>
  (forall (s :: S). Term s (a :--> b)) ->
  pf a b
pattern PFn f <- (unTestableTerm . applyPFun -> f)    
