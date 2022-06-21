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
    -- * Haskell Property Combinators

    -- ** Dual Implementation
    ImplChoice (..),
    dualImplProp,
    testDualImplProperty,
    testDualImplGroup,

    -- ** Input Class Coverage Combinators
    classifiedForAllShrink,
    classifiedForAllShrink',

    -- * Basic Plutarch Input-Dependent Properties
    peqCanFailProperty,
    peqCanFailPropertyNative,
    peqConstCanFailProperty,
    peqConstProperty,
    peqPropertyNative,

    -- * Quantified Plutarch Properties

    -- ** Basic Properties
    peqProperty,

    -- ** Basic Properties with Native Haskell
    peqPropertyNative',

    -- ** Properties that Always Should Fail
    alwaysFailProperty,

    -- ** Coverage-Based Properties
    classifiedProperty,

    -- ** Coverage-Based Properties with Native Haskell
    classifiedPropertyNative,
) where

import Control.Monad (guard)
import Data.Functor ((<&>))
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
    Testable,
    checkCoverage,
    counterexample,
    cover,
    elements,
    forAllShrinkShow,
    property,
 )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Plutarch.Helpers (ourStyle)
import Test.Tasty.QuickCheck (testProperty)
import Text.PrettyPrint (
    hang,
    renderStyle,
    vcat,
 )
import Text.Show.Pretty (ppDoc, ppShow)

-- Haskell Property combinators

--   Dual Implementation

data ImplChoice = Haskell | Plutarch deriving stock (Eq, Ord, Show, Enum, Bounded)

{- | Constructs an input-dependent dual-implementation Property.

 This empowers the dual-implementation strategy: The property only needs to be
 tested on the Haskell implementation, if the outputs of all implementations are
 equal, as long as all inputs are generated in the exact same way. Using this,
 the latter is guaranteed.

 Can be composed with 'classifiedForAllShrink\''.

 Leave the 'ImplChoice' parameter open and make use of 'testDualImplProperty'
 and 'testDualImplGroup', as opposed to 'testProperty' and 'testGroup'.
-}
dualImplProp ::
    forall (a :: Type) (b :: Type) (ra :: S -> Type) (rb :: S -> Type) (prop :: Type).
    ( PLifted ra ~ a
    , PLifted rb ~ b
    , PUnsafeLiftDecl ra
    , PUnsafeLiftDecl rb
    , PEq rb
    , Testable prop
    ) =>
    -- | The Haskell implementation.
    (a -> b) ->
    -- | The Plutarch implementation.
    (forall (s :: S). Term s (ra :--> rb)) ->
    -- | The property.
    (a -> b -> prop) ->
    -- | Choice of implementation.
    ImplChoice ->
    -- | Input value.
    a ->
    Property
dualImplProp haskellImpl plutarchImpl prop =
    \case
        -- TODO could introduce a choice 'Both' to run both in the same Property
        Haskell -> \a -> property $ prop a (haskellImpl a)
        Plutarch -> peqPropertyNative haskellImpl plutarchImpl

-- | Drop-in replacement for 'testProperty'.
testDualImplProperty :: TestName -> (ImplChoice -> Property) -> (ImplChoice -> TestTree)
testDualImplProperty testName = (testProperty testName .)

-- | Drop-in replacement for 'testGroup'.
testDualImplGroup :: TestName -> [ImplChoice -> TestTree] -> ImplChoice -> TestTree
testDualImplGroup testName tests choice = testGroup testName (map ($ choice) tests)

-- Input Class Coverage Combinators

-- TODO they should probably live in another lib, independent of Plutarch
-- TODO custom class for pretty input classes? Show is not inteded for generating pretty messages.

{- | Run a property based on verified input classes, ensuring equal coverage of all classes.

Set up your input class like this:

@
data MyInputClass = TooSmall | JustFine | TooBig
  deriving stock (Eq, Enum, Bounded)

-- the default implementations work fine, they only need 'Enum' and 'Bounded'
instance Universe MyInputClass
instance Finite MyInputClass

-- pretty case distribution message
instance Show MyInputClass where
    show = \case
        TooSmall -> "too small"
        CorrectSize -> "correct size"
        TooBig -> "too big"
@
-}
classifiedForAllShrink ::
    forall (a :: Type) (ix :: Type).
    ( Show a
    , Finite ix
    , Eq ix
    , Show ix
    ) =>
    -- | Class-dependent input generator. Gets verified by the classifier function below.
    (ix -> Gen a) ->
    -- | A \'classifier function\' for generated inputs. Used to verify the generator output.
    (a -> ix) ->
    -- | A shrinker for the generated inputs.
    --
    -- Shrunken inputs that change the input class get filtered out. The class
    -- parameter can be used to increase efficiency by not generating
    -- class-changing shrinks in the first place.
    (ix -> a -> [a]) ->
    -- | The 'Property' depending on the input.
    --
    -- The class parameter can be used to simplify verifying results, for
    -- example if a certain class should always result in failure. It should not
    -- be used by the thing being tested.
    (ix -> a -> Property) ->
    Property
classifiedForAllShrink getGen classify shr prop = case cardinality @ix of
    Tagged 0 -> failOutNoCases
    Tagged 1 -> failOutOneCase
    _ -> forAllShrinkShow gen shr' (showInput . snd) go
  where
    gen :: Gen (ix, a)
    gen = do
        ix <- elements universeF
        (ix,) <$> getGen ix
    shr' :: (ix, a) -> [(ix, a)]
    shr' (ix, x) = do
        x' <- shr ix x
        guard (classify x' == ix)
        pure (ix, x')
    go (inputClass, input) =
        let actualClass = classify input
         in if inputClass /= actualClass
                then failedClassification inputClass actualClass
                else ensureCovered inputClass (prop inputClass input)

{- | Simpler version of 'classifiedForAllShrink' that doesn't pass the class to
 the shrinker and property.
-}
classifiedForAllShrink' ::
    forall (a :: Type) (ix :: Type).
    ( Show a
    , Finite ix
    , Eq ix
    , Show ix
    ) =>
    -- | Class-dependent input generator. Gets verified by the classifier function below.
    (ix -> Gen a) ->
    -- | A \'classifier function\' for generated inputs. Used to verify the generator output.
    (a -> ix) ->
    -- | A shrinker for the generated inputs.
    --
    -- Shrunken inputs that change the input class get filtered out.
    (a -> [a]) ->
    -- | The 'Property' depending on the input.
    (a -> Property) ->
    Property
-- TODO needed?
classifiedForAllShrink' getGen classify shr prop =
    classifiedForAllShrink getGen classify (const shr) (const prop)

-- Basic Plutarch Input-Dependent Properties

{- | Input-dependent equality property on fallible Plutarch functions, using a
 Plutarch expectation function.
-}
peqCanFailProperty ::
    forall (a :: Type) (pa :: S -> Type) (r :: S -> Type).
    ( PEq r
    , PUnsafeLiftDecl pa
    , PLifted pa ~ a
    ) =>
    -- | Constructs the expected result, wrapped in 'PMaybe'. Returns 'PNothing'
    -- to signal expected failure.
    (forall (s :: S). Term s (pa :--> PMaybe r)) ->
    -- | The function being tested.
    (forall (s :: S). Term s (pa :--> r)) ->
    -- | The input for the function, on the Haskell level.
    a ->
    Property
peqCanFailProperty pgetOutcome comp input =
    peqConstCanFailProperty (pgetOutcome # pconstant input) comp input

peqCanFailPropertyNative ::
    forall (a :: Type) (r :: Type) (pa :: S -> Type) (pr :: S -> Type).
    ( PEq pr
    , PUnsafeLiftDecl pa
    , PUnsafeLiftDecl pr
    , PLifted pa ~ a
    , PLifted pr ~ r
    ) =>
    -- | Constructs the expected result on the Haskell level, wrapped in
    -- 'Maybe'. Returns 'Nothing' to signal expected failure.
    (a -> Maybe r) ->
    -- | The function being tested.
    (forall (s :: S). Term s (pa :--> pr)) ->
    -- | The input for the function, on the Haskell level.
    a ->
    Property
peqCanFailPropertyNative getOutcome comp input =
    peqConstCanFailProperty (toPMaybe $ getOutcome input) comp input

{- | Input-dependent equality property on fallible Plutarch functions, using a
 Plutarch expectation function.
-}
peqConstCanFailProperty ::
    forall (a :: Type) (pa :: S -> Type) (r :: S -> Type).
    ( PEq r
    , PUnsafeLiftDecl pa
    , PLifted pa ~ a
    ) =>
    -- | The expected result, wrapped in 'PMaybe'. Returns 'PNothing'
    -- to signal expected failure.
    (forall (s :: S). Term s (PMaybe r)) ->
    -- | The function being tested.
    (forall (s :: S). Term s (pa :--> r)) ->
    -- | The input for the function, on the Haskell level.
    a ->
    Property
peqConstCanFailProperty pexpected comp input =
    counterexample (prettyLogs logs) $ handleScriptResult res pexpected
  where
    script = compile (ppredCompareCanFailTemplate (#==) pexpected (comp # pconstant input))
    (res, _, logs) = evalScript script

{- | Input-dependent equality property on Plutarch functions that are not
 expected to fail, using a constant Plutarch term for the expected value.
-}
peqConstProperty ::
    forall (a :: Type) (c :: S -> Type) (d :: S -> Type).
    (PLifted c ~ a, PEq d, PUnsafeLiftDecl c) =>
    -- | The expected result.
    (forall (s :: S). Term s d) ->
    -- | The function being tested.
    (forall (s :: S). Term s (c :--> d)) ->
    -- | The input for the function, on the Haskell level.
    a ->
    Property
peqConstProperty expected comp input =
    let s = compile (expected #== comp # pconstant input)
        (res, _, logs) = evalScript s
     in counterexample (prettyLogs logs) $ case res of
            Left e -> unexpectedError e
            Right s' -> sameAsExpected s'

{- | Input-dependent equality property on Plutarch functions that are not
 expected to fail, using a Haskell function to construct the expected value.
-}
peqPropertyNative ::
    forall (a :: Type) (b :: Type) (c :: S -> Type) (d :: S -> Type).
    ( PLifted c ~ a
    , PLifted d ~ b
    , PEq d
    , PUnsafeLiftDecl c
    , PUnsafeLiftDecl d
    ) =>
    -- | Given an input value, returns the expected value.
    (a -> b) ->
    -- | The function being tested.
    (forall (s :: S). Term s (c :--> d)) ->
    -- | The input for the function, on the Haskell level.
    a ->
    Property
peqPropertyNative getExpected comp input =
    peqConstProperty (pconstant $ getExpected input) comp input

-- Quantified Plutarch Properties

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
-- TODO this should probably just be thrown out
peqProperty expected gen shr comp =
    forAllShrinkShow gen shr showInput (peqConstProperty expected comp)

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
-- TODO this should probably just be thrown out
peqPropertyNative' getExpected gen shr comp =
    forAllShrinkShow gen shr showInput (peqPropertyNative getExpected comp)

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
    --
    -- Shrunken inputs that change the input class get filtered out.
    (a -> [a]) ->
    -- | Given a Plutarch equivalent to an input, constructs its corresponding
    -- expected result. Returns 'PNothing' to signal expected failure.
    (forall (s :: S). Term s (c :--> PMaybe d)) ->
    -- | A \'classifier function\' for generated inputs.
    (a -> ix) ->
    -- | The computation to test.
    (forall (s :: S). Term s (c :--> d)) ->
    Property
-- TODO make input order like in classifiedForAllShrink?
-- TODO allow class dependent shrinker and expecter?
-- TODO this should probably just be thrown out, users can compose those two functions themselves
classifiedProperty getGen shr getOutcome classify comp =
    classifiedForAllShrink' getGen classify shr (peqCanFailProperty getOutcome comp)

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
    --
    -- Shrunken inputs that change the input class get filtered out.
    (a -> [a]) ->
    -- | Given an input value, constructs its corresponding expected result.
    -- Returns 'Nothing' to signal expected failure.
    (a -> Maybe e) ->
    -- | A \'classifier function\' for generated inputs.
    (a -> ix) ->
    -- | The computation to test.
    (forall (s :: S). Term s (c :--> d)) ->
    Property
-- TODO make input order like in classifiedForAllShrink?
-- TODO allow class dependent shrinker and expecter?
-- TODO this should probably just be thrown out, users can compose those two functions themselves
classifiedPropertyNative getGen shr getOutcome classify comp =
    classifiedForAllShrink' getGen classify shr (peqCanFailPropertyNative getOutcome comp)

-- Note from Koz
--
-- The 'double lift' in the above definition is definitely quite suboptimal.
-- However, there seems to be no way to let-bind the result of pconstant while
-- simultaneously convincing GHC it's closed.

-- Templates

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
ppredCompareCanFailTemplate ::
    forall (d :: S -> Type) (e :: S -> Type) (s :: S).
    -- | The predicate, used to judge the computation result by the comparison value.
    (forall (s' :: S). Term s' d -> Term s' e -> Term s' PBool) ->
    -- | Comparison value wrapped in 'PMaybe'. 'PNothing' to signal expected failure.
    (forall (s' :: S). Term s' (PMaybe d)) ->
    -- | The computation to test.
    (forall (s' :: S). Term s' e) ->
    Term s PInteger
ppredCompareCanFailTemplate ppredicate mayComparisonVal comp = unTermCont $ do
    actual <- pletC comp
    pmatchC mayComparisonVal <&> \case
        PNothing -> 2
        PJust comparisonVal -> pif (ppredicate comparisonVal actual) 0 1

-- Property handlers

-- | Handler for 'peqCanFailTemplate'.
handleScriptResult :: forall d. Either EvalError Script -> (forall (s :: S). Term s (PMaybe d)) -> Property
handleScriptResult res pexpected =
    case res of
        Right retCode ->
            if
                    | retCode == canon 2 -> counterexample ranOnCrash . property $ False
                    | retCode == canon 0 -> property True
                    | otherwise -> counterexample wrongResult . property $ False
        Left e ->
            let sIsCrashExpected = compile (pisNothing #$ pexpected)
                (testRes, _, _) = evalScript sIsCrashExpected
             in case testRes of
                    Left e' -> failCrashyGetOutcome e'
                    Right isCrashExpected -> handleCrashForExpectation e isCrashExpected

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

-- Missing Plutarch functions. Candidates for liqwid-plutarch-extra?

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
