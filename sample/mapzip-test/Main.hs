{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental
 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Prelude (PBool, PBuiltinList, PList, Term, pfilter, plam, pmap, (#), (#$), (#==), (:-->))
import Plutarch.Test.QuickCheck (PA, fromPFun, lifty)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.QuickCheck (Property, QuickCheckTests, arbitrary, forAllShrink, resize, shrink, testProperty)

mapzipProp :: Property
mapzipProp = forAllShrink arbitrary shrink $ fromPFun test
  where
    test ::
      Term
        s
        ( (PA :--> PBool)
            :--> (PA :--> PA)
            :--> PBuiltinList PA
            :--> PBool
        )
    test = plam $ \f g x ->
      pfilter # f # (pmap # g # x) #== pmap # g # (pfilter # f # x)

composeProp :: Property
composeProp =
  forAllShrink lifty shrink $ \x ->
  forAllShrink lifty shrink $ \y ->
    forAllShrink (resize 20 arbitrary) shrink $ \z ->
      (fromPFun test) x y z
  where
    test ::
      Term
        s
        ( (PA :--> PA)
            :--> (PA :--> PA)
            :--> PList PA
            :--> PBool
        )
    test = plam $ \f g x ->
      (pmap # g #$ pmap # f # x) #== (pmap # plam (\y -> g #$ f # y) # x)

main :: IO ()
main = do
  -- This will fix some problems regarding text encoding.
  setLocaleEncoding utf8
  defaultMain . adjustOption go $
    testGroup
      ""
      [ expectFail $ testProperty "map zip is equal to zip map" mapzipProp
      , testProperty "composition should be eqaul to separate applications" composeProp
      ]
  where
    -- 100 tests is way too small for a property test to search for a counterexample,
    -- it is recommanded to use at least 10,000. However, more is better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 1000
