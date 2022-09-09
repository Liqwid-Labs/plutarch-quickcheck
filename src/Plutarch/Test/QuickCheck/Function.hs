{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck.Function (
  WrapPFun (..),
  lifty,
  unlifty,
  PFun (..),
  PFunLift (..),
  PFunction (..),
  pattern PFn,
  pfunTable,
  pfunFinite,
  pfunLiftTable,
  pfunLiftFinite,
) where

import Plutarch.Test.QuickCheck.Function.Internal (PFunction (..))
import Plutarch.Test.QuickCheck.Function.PFun (PFun (..), pfunFinite, pfunTable)
import Plutarch.Test.QuickCheck.Function.PFunLift (PFunLift (..), pfunLiftFinite, pfunLiftTable)
import Plutarch.Test.QuickCheck.Instances (TestableTerm (unTestableTerm))

import Plutarch.Prelude (S, Term, Type, (:-->))
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
 )

{- | An abstraction over function generators. It allows flexible use
     of two different generators via existential type.

     By default, 'WrapPFun' will perform 'unlifty' function
     generation. However, targetting function is a simple function
     with only 'PLift' types, like @(PA :--> PA)@, user should
     override it with 'lifty' generator by using 'forAll' or similar.

 @since 2.2.0
-}
data WrapPFun (a :: S -> Type) (b :: S -> Type)
  = forall pf. (PFunction pf, Arbitrary (pf a b), Show (pf a b)) => WrapPFun (pf a b)

instance PFunction WrapPFun where
  applyPFun (WrapPFun pf) = applyPFun pf

-- | @since 2.2.0
instance
  forall (a :: S -> Type) (b :: S -> Type).
  PFunctionConstraint PFun a b =>
  Arbitrary (WrapPFun a b)
  where
  arbitrary = WrapPFun <$> (arbitrary :: Gen (PFun a b))
  shrink (WrapPFun x) = WrapPFun <$> shrink x

-- | @since 2.2.0
instance forall (a :: S -> Type) (b :: S -> Type). Show (WrapPFun a b) where
  show (WrapPFun x) = show x

{- | Generator for function generator that lifts, meaning it requires
     'PLift' instances for both input and output type. When compared
     to 'unlifty', it is significantly faster as it uses Data encoded
     list to represent the function.

     When possible, it is recommended to use 'lifty' function
     generation due to significant performance improvement.

 @since 2.2.0
-}
lifty ::
  forall (a :: S -> Type) (b :: S -> Type).
  PFunctionConstraint PFunLift a b =>
  Gen (WrapPFun a b)
lifty = WrapPFun <$> (arbitrary :: Gen (PFunLift a b))

{- | Generator for function generator that does not lift, meaning that
     this generator will generate any value in category of Plut--PPair
     for multi-parameter function, Scott-encoded PMaybe, any types.

     Downside being slower speed. To store any Plut, it uses
     Scott-encoded list internally to represent the function. It takes
     up more memory and cpu budgets.

     When possible, use 'lifty'.

 @since 2.2.0
-}
unlifty ::
  forall (a :: S -> Type) (b :: S -> Type).
  PFunctionConstraint PFun a b =>
  Gen (WrapPFun a b)
unlifty = arbitrary

{-# COMPLETE PFn #-}
pattern PFn ::
  forall (a :: S -> Type) (b :: S -> Type) (pf :: (S -> Type) -> (S -> Type) -> Type).
  PFunction pf =>
  (forall (s :: S). Term s (a :--> b)) ->
  pf a b
pattern PFn f <- (unTestableTerm . applyPFun -> f)
