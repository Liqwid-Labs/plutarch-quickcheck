{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Module: Test.Tasty.Plutarch.Property
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Helpers for @tasty-quickcheck@ to write property tests for Plutarch.
-}
module Test.Tasty.Plutarch.Property (
    -- * Basic properties
    peqProperty,

    -- * Basic properties with native Haskell
    peqPropertyNative',

    -- * Properties that always should fail
    alwaysFailProperty,

    -- * Coverage-based properties
    classifiedProperty,

    -- * Coverage-based properties with native Haskell
    classifiedPropertyNative,
) where

import Control.Monad (guard)
import Data.Kind (Type)
import Data.Monoid (Endo (Endo), appEndo)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Universe (Finite (cardinality, universeF))
import Plutarch (
    S,
    Term,
    compile,
    pcon,
    phoistAcyclic,
    plam,
    pmatch,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Bool (PBool (PFalse, PTrue), PEq ((#==)), pif)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted), pconstant)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import PlutusLedgerApi.V1.Scripts (Script)
import Test.QuickCheck (
    Gen,
    Property,
    checkCoverage,
    counterexample,
    cover,
    elements,
    forAllShrinkShow,
    property,
 )
import Test.Tasty.Plutarch.Helpers (ourStyle)
import Text.PrettyPrint (
    hang,
    renderStyle,
    vcat,
 )
import Text.Show.Pretty (ppDoc, ppShow)

{- | Given an expected result, and a generator and shrinker for inputs, run the
 given computation on the generated input, ensuring that it always matches the
 expected result (according to 'PEq').

 = Note

 By default, QuickCheck will only run 100 tests. This is /nowhere/ near enough
 for almost any property; ensure that you use @tasty-quickcheck@'s test count
 option to increase this to at /least/ several thousand.

 @since 1.0.0
-}
peqProperty ::
    forall (a :: Type) (c :: S -> Type) (d :: S -> Type).
    (Show a, PLifted c ~ a, PEq d, PUnsafeLiftDecl c) =>
    (forall (s :: S). Term s d) ->
    Gen a ->
    (a -> [a]) ->
    (forall (s :: S). Term s (c :--> d)) ->
    Property
peqProperty expected gen shr comp =
    forAllShrinkShow gen shr showInput (go (peqTemplate comp))
  where
    go ::
        (forall (s' :: S). Term s' (d :--> c :--> PBool)) ->
        a ->
        Property
    go precompiled input =
        let s = compile (precompiled # expected # pconstant input)
            (res, _, logs) = evalScript s
         in counterexample (prettyLogs logs) $ case res of
                Left e -> unexpectedError e
                Right s' -> sameAsExpected s'

{- | Have the same functionalities as 'peqProperty' but allow generating
 expeceted result using a Haskell function.

 @since 1.0.2
-}
peqPropertyNative' ::
    forall (a :: Type) (b :: Type) (c :: S -> Type) (d :: S -> Type).
    ( Show a
    , PLifted c ~ a
    , PLifted d ~ b
    , PEq d
    , PUnsafeLiftDecl c
    , PUnsafeLiftDecl d
    ) =>
    -- | Given an input value, returns the expected value.
    (a -> b) ->
    Gen a ->
    (a -> [a]) ->
    (forall (s :: S). Term s (c :--> d)) ->
    Property
peqPropertyNative' getExpected gen shr comp =
    forAllShrinkShow gen shr showInput (go (peqTemplate comp))
  where
    go ::
        (forall (s' :: S). Term s' (d :--> c :--> PBool)) ->
        a ->
        Property
    go precompiled input =
        let expected :: Term s' d
            expected = pconstant $ getExpected input
            s = compile (precompiled # expected # pconstant input)
            (res, _, logs) = evalScript s
         in counterexample (prettyLogs logs) $ case res of
                Left e -> unexpectedError e
                Right s' -> sameAsExpected s'

{- | 'alwaysFailProperty' universally checks if given computation fails.
 This runs the given script with generated input; it makes sure that
 script fails by crashes or errors.

 This function provides quicker and simpler interface when writing
 properties involving, for example, a validator or a minting policy
 because these scripts are focused on correctly aborting script when
 certain input is given.

 @since 1.0.1
-}
alwaysFailProperty ::
    forall (a :: Type) (c :: S -> Type) (d :: S -> Type).
    ( Show a
    , PLifted c ~ a
    , PUnsafeLiftDecl c
    ) =>
    Gen a ->
    (a -> [a]) ->
    (forall (s :: S). Term s (c :--> d)) ->
    Property
alwaysFailProperty gen shr comp = forAllShrinkShow gen shr showInput (go comp)
  where
    go :: (forall (s' :: S). Term s' (c :--> d)) -> a -> Property
    go precompiled input =
        let s = compile (precompiled # pconstant input)
            (res, _, _) = evalScript s
         in case res of
                Right _ -> counterexample ranOnCrash . property $ False
                Left _ -> property True

{- | Given a finite set of classes, each with an associated generator of inputs,
 a shrinker for inputs, a Plutarch function for constructing (possible)
 expected results, and a way of classifying any generated input, run the given
 computation enough times to ensure that all the following hold:

 * No case behaves contrary to its expected result; if no expected result exists
   for a given input, the computation being tested must error.
 * Equal numbers of all possible cases are generated.
 * Running more tests would not impact the distribution of cases (as per
   above) very much.

 We use QuickCheck's coverage support to ensure all the above hold. In
 particular, this means any request for an exact number of tests to run will
 be ignored.

 = Note

 The cardinality of @ix@ matters significantly; the larger it is, the longer
 the property you specify here will take to run. If the cardinality of @ix@ is
 zero or one, the 'Property' will fail before anything is generated.

 Cases are labelled according to the 'Show' instance of @ix@; thus, we
 recommend that you define a custom type with the necessary instances for
 any given property.

 @since 1.0.0
-}
classifiedProperty ::
    forall (a :: Type) (ix :: Type) (c :: S -> Type) (d :: S -> Type).
    ( Show a
    , Finite ix
    , Eq ix
    , Show ix
    , PLifted c ~ a
    , PUnsafeLiftDecl c
    , PEq d
    ) =>
    -- | A way to generate input values for each case. We expect that for any
    -- such generated value, the classification function should report the
    -- appropriate case: a test iteration will fail if this doesn't happen.
    (ix -> Gen a) ->
    -- | A shrinker for inputs.
    (a -> [a]) ->
    -- | Given a Plutarch equivalent to an input, constructs its corresponding
    -- expected result. Returns 'PNothing' to signal expected failure.
    (forall (s :: S). Term s (c :--> PMaybe d)) ->
    -- | A \'classifier function\' for generated inputs.
    (a -> ix) ->
    -- | The computation to test.
    (forall (s :: S). Term s (c :--> d)) ->
    Property
classifiedProperty getGen shr getOutcome classify comp = case cardinality @ix of
    Tagged 0 -> failOutNoCases
    Tagged 1 -> failOutOneCase
    _ -> forAllShrinkShow gen shr' (showInput . snd) (go (ppropertyTemplate comp getOutcome))
  where
    gen :: Gen (ix, a)
    gen = do
        ix <- elements universeF
        (ix,) <$> getGen ix
    shr' :: (ix, a) -> [(ix, a)]
    shr' (ix, x) = do
        x' <- shr x
        guard (classify x' == ix)
        pure (ix, x')
    go ::
        (forall (s' :: S). Term s' (c :--> PInteger)) ->
        (ix, a) ->
        Property
    go pproperty (inputClass, input) =
        let actualClass = classify input
         in if inputClass /= actualClass
                then failedClassification inputClass actualClass
                else
                    let s = compile (pproperty # pconstant input)
                        (res, _, logs) = evalScript s
                     in counterexample (prettyLogs logs)
                            . ensureCovered inputClass
                            $ handleScriptResult res input
    handleScriptResult :: Either EvalError Script -> a -> Property
    handleScriptResult res input =
        case res of
            Right retCode ->
                if
                        | retCode == canon 2 -> counterexample ranOnCrash . property $ False
                        | retCode == canon 0 -> property True
                        | otherwise -> counterexample wrongResult . property $ False
            Left e ->
                let sTest = compile (pisNothing #$ getOutcome # pconstant input)
                    (testRes, _, _) = evalScript sTest
                 in case testRes of
                        Left e' -> failCrashyGetOutcome e'
                        Right isCrashExpected ->
                            handleCrashForExpectation e isCrashExpected

{- | Identical to @classifiedProperty@ but it receives expected result
   in Haskell function instead of Plutarch function. As a result it
   requires little bit tighter constraints but allows more readable
   code for writing expected result.

 @since 1.0.2
-}
classifiedPropertyNative ::
    forall (a :: Type) (e :: Type) (ix :: Type) (c :: S -> Type) (d :: S -> Type).
    ( Show a
    , Finite ix
    , Eq ix
    , Show ix
    , PLifted c ~ a
    , PLifted d ~ e
    , PUnsafeLiftDecl c
    , PUnsafeLiftDecl d
    , PEq d
    ) =>
    -- | A way to generate values for each case. We expect that for any such
    -- generated value, the classification function should report the appropriate
    -- case: a test iteration will fail if this doesn't happen.
    (ix -> Gen a) ->
    -- | A shrinker for inputs.
    (a -> [a]) ->
    -- | Given an input value, constructs its corresponding expected result.
    -- Returns 'Nothing' to signal expected failure.
    (a -> Maybe e) ->
    -- | A \'classifier function\' for generated inputs.
    (a -> ix) ->
    -- | The computation to test.
    (forall (s :: S). Term s (c :--> d)) ->
    Property
classifiedPropertyNative getGen shr getOutcome classify comp = case cardinality @ix of
    Tagged 0 -> failOutNoCases
    Tagged 1 -> failOutOneCase
    _ -> forAllShrinkShow gen shr' (showInput . snd) (go (ppropertyTemplateNativeEx comp))
  where
    gen :: Gen (ix, a)
    gen = do
        ix <- elements universeF
        (ix,) <$> getGen ix
    shr' :: (ix, a) -> [(ix, a)]
    shr' (ix, x) = do
        x' <- shr x
        guard (classify x' == ix)
        pure (ix, x')
    go ::
        (forall (s' :: S). Term s' (c :--> PMaybe d :--> PInteger)) ->
        (ix, a) ->
        Property
    go pproperty (inputClass, input) =
        let actualClass = classify input
         in if inputClass /= actualClass
                then failedClassification inputClass actualClass
                else
                    let s = compile (pproperty # pconstant input # toPMaybe (getOutcome input))
                        (res, _, logs) = evalScript s
                     in counterexample (prettyLogs logs)
                            . ensureCovered inputClass
                            $ handleScriptResult res input
    handleScriptResult :: Either EvalError Script -> a -> Property
    handleScriptResult res input =
        case res of
            Right retCode ->
                if
                        | retCode == canon 2 -> counterexample ranOnCrash . property $ False
                        | retCode == canon 0 -> property True
                        | otherwise -> counterexample wrongResult . property $ False
            Left e ->
                let sIsCrashExpected = compile (pisNothing #$ toPMaybe (getOutcome input))
                    (testRes, _, _) = evalScript sIsCrashExpected
                 in case testRes of
                        Left e' -> failCrashyGetOutcome e'
                        Right isCrashExpected -> handleCrashForExpectation e isCrashExpected

-- Note from Koz
--
-- The 'double lift' in the above definition is definitely quite suboptimal.
-- However, there seems to be no way to let-bind the result of pconstant while
-- simultaneously convincing GHC it's closed.

-- Helpers

-- Templates

peqTemplate ::
    forall (c :: S -> Type) (d :: S -> Type) (s :: S).
    (PEq d) =>
    (forall (s' :: S). Term s' (c :--> d)) ->
    Term s (d :--> c :--> PBool)
peqTemplate comp = phoistAcyclic $
    plam $ \expected input ->
        expected #== comp # input

{- | The resulting Plutarch function tests the given computation.

 Note from Seungheon!
 Here, Template is using old fashion C-style
 error handler--it uses integer for different types
 of possibilities. We do not want to use custom datatype
 or PMaybe as it will either have Scott-encoded weirdness
 or much longer code.

 0 - failure
 1 - success
 2 - unexpected success

 Due to Plutarch weird-ness, probably, Scott-encoded
 negative Integers, all "codes" should be positive number.
-}
ppropertyTemplate ::
    forall (c :: S -> Type) (d :: S -> Type) (s :: S).
    (PEq d) =>
    -- | The computation to test.
    (forall (s' :: S). Term s' (c :--> d)) ->
    -- | Given a Plutarch equivalent to an input, constructs its corresponding
    -- expected result. Returns 'PNothing' to signal expected failure.
    (forall (s' :: S). Term s' (c :--> PMaybe d)) ->
    Term s (c :--> PInteger)
ppropertyTemplate comp getOutcome = phoistAcyclic $
    plam $ \input -> unTermCont $ do
        actual <- pletC (comp # input)
        expectedMay <- pmatchC (getOutcome # input)
        pure $ case expectedMay of
            PNothing -> 2
            PJust expected -> pif (expected #== actual) 0 1

{- | The resulting Plutarch function tests the computation, given the input and
 expected outcome. For return codes see 'ppropertyTemplate'.
-}
ppropertyTemplateNativeEx ::
    forall (c :: S -> Type) (d :: S -> Type) (s :: S).
    (PEq d) =>
    -- | The computation to test.
    (forall (s' :: S). Term s' (c :--> d)) ->
    Term s (c :--> PMaybe d :--> PInteger)
ppropertyTemplateNativeEx comp = phoistAcyclic $
    plam $ \input res -> unTermCont $ do
        actual <- pletC (comp # input)
        expectedMay <- pmatchC res
        pure $ case expectedMay of
            PNothing -> 2
            PJust expected -> pif (expected #== actual) 0 1

toPMaybe ::
    forall (a :: Type) (c :: S -> Type) (s :: S).
    ( PLifted c ~ a
    , PUnsafeLiftDecl c
    ) =>
    Maybe a ->
    Term s (PMaybe c)
toPMaybe (Just x) = pcon $ PJust $ pconstant x
toPMaybe Nothing = pcon PNothing

pisNothing ::
    forall (a :: S -> Type) (s :: S).
    Term s (PMaybe a :--> PBool)
pisNothing = phoistAcyclic $
    plam $ \t -> pmatch t $ \case
        PNothing -> pcon PTrue
        PJust _ -> pcon PFalse

-- Property handlers

ranOnCrash :: String
ranOnCrash = "A case which should have crashed ran successfully instead."

wrongResult :: String
wrongResult = "Test script result does not match expected value."

failCrashyGetOutcome :: EvalError -> Property
failCrashyGetOutcome err = counterexample go . property $ False
  where
    go :: String
    go =
        renderStyle ourStyle $
            "Unexpected crash when constructing expected value.\n"
                <> hang "Error details" 4 (ppDoc err)

failOutNoCases :: Property
failOutNoCases = counterexample go . property $ False
  where
    go :: String
    go = "The set of classes is empty; giving up."

failOutOneCase :: Property
failOutOneCase = counterexample go . property $ False
  where
    go :: String
    go =
        "The set of classes is a singleton.\n"
            <> "Use peqProperty instead."

unexpectedError :: EvalError -> Property
unexpectedError err = counterexample go . property $ False
  where
    go :: String
    go =
        renderStyle ourStyle $
            "Unexpected error when running test script.\n"
                <> hang "Error details" 4 (ppDoc err)

-- TODO: Figure out a way of capturing the result of just the function being
-- passed to the test.
sameAsExpected :: Script -> Property
sameAsExpected actual = counterexample wrongResult (canonTrue == actual)

handleCrashForExpectation :: EvalError -> Script -> Property
handleCrashForExpectation err isCrashExpected =
    counterexample go (canonTrue == isCrashExpected)
  where
    go :: String
    go =
        renderStyle ourStyle $
            "Expected a successful run, but crashed instead.\n"
                <> hang "Error details" 4 (ppDoc err)

failedClassification ::
    forall (ix :: Type).
    (Show ix) =>
    ix ->
    ix ->
    Property
failedClassification expected actual = counterexample go . property $ False
  where
    go :: String
    go =
        renderStyle ourStyle $
            "Unexpected mis-classification.\n"
                <> "This can happen when you have a faulty generator or shrinker.\n"
                <> hang "Expected classification" 4 (ppDoc expected)
                <> "\n"
                <> hang "Actual classification" 4 (ppDoc actual)

-- Others

showInput :: forall (a :: Type). (Show a) => a -> String
showInput x = renderStyle ourStyle $ hang "Generated input" 4 (ppDoc x)

ensureCovered ::
    forall (ix :: Type).
    (Finite ix, Eq ix, Show ix) =>
    ix ->
    Property ->
    Property
ensureCovered inputClass =
    fmap checkCoverage . appEndo . foldMap (Endo . go) $ universeF
  where
    go :: ix -> Property -> Property
    go ix = cover probability (inputClass == ix) (ppShow ix)
    probability :: Double
    probability =
        let Tagged k = cardinality @ix
         in 100.0 / fromIntegral k

canonTrue :: Script
canonTrue = compile (pcon PTrue)

canon :: Integer -> Script
canon x = compile (pconstant x)

prettyLogs :: [Text] -> String
prettyLogs =
    renderStyle ourStyle . \case
        [] -> "No logs found.\n" <> "Did you forget to build Plutarch with +development?"
        logs -> hang "Logs" 4 (vcat . fmap ppDoc $ logs)
