{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck.Function.PFunLift (
  PFunLift (..),
  pfunLiftTable,
  pfunLiftFinite,
) where

import Control.Arrow (Arrow ((&&&)))
import Data.Kind (Type)
import Data.List (intercalate, nubBy)
import Data.Universe (Finite (universeF))
import Plutarch (S, Term, plam, (#), (#$), type (:-->))
import Plutarch.Extra.Maybe (pmaybe)
import Plutarch.Lift (PLift, PUnsafeLiftDecl (PLifted), pconstant)
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude (
  PBuiltinList,
  PBuiltinPair,
  PEq,
  PIsListLike,
  PMaybe (PJust, PNothing),
  pcon,
  pfind,
  pfstBuiltin,
  phoistAcyclic,
  pmatch,
  psndBuiltin,
  (#==),
 )
import Plutarch.Test.QuickCheck.Instances (TestableTerm (TestableTerm))
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Gen,
  sized,
  vectorOf,
 )

import Plutarch.Test.QuickCheck.Function.Internal (PFunction (..))

{- | Same as 'PFun', but uses Data-encoded `PBuiltinList`. With its
     stricter constraint, it has faster performance over 'PFun'. It
     will only allow types that can be `PLifted`. If one needs to
     generate anything not `PLift` type, they should use `PFun`.

 @since 2.2.0
-}
data PFunLift (a :: S -> Type) (b :: S -> Type) where
  PFunLift ::
    (PLift a, PLift b) =>
    [(PLifted a, PLifted b)] ->
    PLifted b ->
    (TestableTerm (a :--> b)) ->
    PFunLift a b

instance PFunction PFunLift where
  type
    PFunctionConstraint PFunLift a b =
      ( PLift a
      , PLift b
      , Arbitrary (PLifted a)
      , Arbitrary (PLifted b)
      , CoArbitrary (PLifted a)
      , Eq (PLifted a)
      , PEq a
      , Show (PLifted a)
      , Show (PLifted b)
      )
  applyPFun (PFunLift _ _ f) = f

mkPFunLift ::
  forall (a :: S -> Type) (b :: S -> Type).
  ( PLift a
  , PLift b
  , PEq a
  ) =>
  [(PLifted a, PLifted b)] ->
  PLifted b ->
  PFunLift a b
mkPFunLift t d = PFunLift t d $ TestableTerm $ pfunLiftTable t d

instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PLift a
  , PLift b
  , Arbitrary (PLifted a)
  , Arbitrary (PLifted b)
  , CoArbitrary (PLifted a)
  , Eq (PLifted a)
  , PEq a
  ) =>
  Arbitrary (PFunLift a b)
  where
  arbitrary = sized $ \r -> do
    xs <- vectorOf r (arbitrary :: Gen (PLifted a))
    ys <- traverse (($ (arbitrary :: Gen (PLifted b))) . coarbitrary) xs
    let table = zip xs ys

    d <- arbitrary :: Gen (PLifted b)
    return $ mkPFunLift (nubBy (\x y -> fst x == fst y) table) d

  shrink (PFunLift t d _) =
    [mkPFunLift t' d' | (t', d') <- shrink (t, d)]

instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PLift a
  , PLift b
  , Show (PLifted a)
  , Show (PLifted b)
  ) =>
  Show (PFunLift a b)
  where
  show = showPFunLift

showPFunLift ::
  forall (a :: S -> Type) (b :: S -> Type).
  ( PLift a
  , PLift b
  , Show (PLifted a)
  , Show (PLifted b)
  ) =>
  PFunLift a b ->
  String
showPFunLift (PFunLift t d _) =
  "{\n"
    ++ intercalate
      ", \n"
      ( [ show x ++ " -> " ++ show c
        | (x, c) <- t
        ]
          ++ ["_ -> " ++ show d]
      )
    ++ "\n}"

pfunLiftTable ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PLift a
  , PLift b
  , PEq a
  ) =>
  [(PLifted a, PLifted b)] ->
  PLifted b ->
  Term s (a :--> b)
pfunLiftTable t d = plam $ \x -> pmaybe # pconstant d # (plookup # x # pconstant t)

pfunLiftFinite ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( Finite (PLifted a)
  , PLift a
  , PLift b
  , PEq a
  ) =>
  (PLifted a -> PLifted b) ->
  Term s (a :--> b)
pfunLiftFinite f = plam $ \x -> pfromJust #$ plookup # x # table
  where
    table :: Term s (PBuiltinList (PBuiltinPair a b))
    table = pconstant $ (id &&& f) <$> universeF

plookup ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind # plam (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))
