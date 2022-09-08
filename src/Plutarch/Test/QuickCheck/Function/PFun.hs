{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Test.QuickCheck.Function.PFun (
  PFun (..),
  applyPFun,
  pfunTable,
  pfunFinite,
) where

import Control.Arrow (Arrow ((&&&)))
import Data.Default (def)
import Data.Kind (Type)
import Data.List (intercalate, nubBy)
import Data.Universe (Finite (universeF))
import Plutarch (S, Term, compile, plam, (#), (#$), type (:-->))
import Plutarch.Extra.Maybe (pmaybe)
import Plutarch.Lift (PLift, PUnsafeLiftDecl (PLifted))
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude (
  PEq,
  PIsListLike,
  PList,
  PMaybe (PJust, PNothing),
  PPair (PPair),
  pcon,
  pcons,
  pfind,
  phoistAcyclic,
  pmatch,
  pnil,
  (#==),
 )
import Plutarch.Show (PShow)
import Plutarch.Test.QuickCheck.Instances (
  PArbitrary (..),
  PCoArbitrary (..),
  TestableTerm (
    TestableTerm,
    unTestableTerm
  ),
  pconstantT,
 )
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  sized,
  vectorOf,
 )

import Plutarch.Test.QuickCheck.Function.Internal (PFunction(..))

data PFun (a :: S -> Type) (b :: S -> Type) where
  PFun ::
    [(TestableTerm a, TestableTerm b)] ->
    TestableTerm b ->
    (TestableTerm (a :--> b)) ->
    PFun a b

instance PFunction PFun where
  type PFunctionConstraint PFun a b =
    ( PArbitrary a
    , PArbitrary b
    , PCoArbitrary a
    , PEq a
    , PShow a
    , PShow b
    )
  applyPFun (PFun _ _ f) = f

mkPFun ::
  forall (a :: S -> Type) (b :: S -> Type).
  PEq a =>
  [(TestableTerm a, TestableTerm b)] ->
  TestableTerm b ->
  PFun a b
mkPFun t d = PFun t d $ TestableTerm $ pfunTable t d

instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PArbitrary a
  , PArbitrary b
  , PCoArbitrary a
  , PEq a
  ) =>
  Arbitrary (PFun a b)
  where
  arbitrary =
    sized $ \r -> do
      xs' <- vectorOf r parbitrary
      let xs = nubBy compScript xs'
      ys <- traverse (($ parbitrary) . coarbitrary) xs

      d <- parbitrary
      return $ mkPFun (zip xs ys) d
    where
      compScript (TestableTerm (compile def -> x)) (TestableTerm (compile def -> y)) = x == y

  shrink (PFun t d _) =
    [mkPFun t' d' | (t', d') <- shrink (t, d)]

instance
  forall (a :: S -> Type) (b :: S -> Type).
  ( PShow a
  , PShow b
  ) =>
  Show (PFun a b)
  where
  show = showPFun

showPFun ::
  forall (a :: S -> Type) (b :: S -> Type).
  ( PShow a
  , PShow b
  ) =>
  PFun a b ->
  String
showPFun (PFun t d _) =
  "{\n"
    ++ intercalate
      ", \n"
      ( [ show x ++ " -> " ++ show c
        | (x, c) <- t
        ]
          ++ ["_ -> " ++ show d]
      )
    ++ "\n}"

-- Puts Haskell list of paris into Soctt Pluts
-- It uses Scott Plut to support not only `PLift` types but also
-- any other Pluts. Using `PBuilinList` wouldn't allow anything but
-- `PLift`s.
conTable ::
  forall (a :: S -> Type) (b :: S -> Type).
  [(TestableTerm a, TestableTerm b)] ->
  TestableTerm (PList (PPair a b))
conTable = foldr go (TestableTerm pnil)
  where
    go :: (TestableTerm a, TestableTerm b) -> TestableTerm (PList (PPair a b)) -> TestableTerm (PList (PPair a b))
    go (TestableTerm x, TestableTerm y) (TestableTerm ps) = TestableTerm $ pcons # pcon (PPair x y) # ps

pfunTable ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PEq a =>
  [(TestableTerm a, TestableTerm b)] ->
  TestableTerm b ->
  Term s (a :--> b)
pfunTable (conTable -> TestableTerm t) (TestableTerm d) =
  plam $ \x -> pmaybe # d # (plookup # x # t)

pfunFinite ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( Finite (PLifted a)
  , PLift a
  , PLift b
  , PEq a
  ) =>
  (PLifted a -> PLifted b) ->
  Term s (a :--> b)
pfunFinite f = plam $ \x -> pfromJust #$ plookup # x # table
  where
    table' = (pconstantT &&& (pconstantT . f)) <$> universeF
    table = unTestableTerm $ conTable table'

plookup ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
  (PEq a, PIsListLike list (PPair a b)) =>
  Term s (a :--> list (PPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind # plam (\p -> pfstPair # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndPair # p))

pfstPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PPair a b :--> a)
pfstPair = phoistAcyclic $ plam $ flip pmatch $ \(PPair x _) -> x

psndPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PPair a b :--> b)
psndPair = phoistAcyclic $ plam $ flip pmatch $ \(PPair _ x) -> x
