{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ADD.Scavenger.Algebra2.InputFilterSpec where

import ADD.Scavenger.Algebra2.InputFilter
  ( HasFilter (CustomFilter, filterMatches),
    InputFilter,
    always,
    andF,
    custom,
    matches,
    never,
    notF,
    orF,
  )
import Data.Data (Proxy (Proxy))
import Data.Foldable (traverse_)
import Data.Word (Word8)
import GHC.Generics (Generic)
import QuickSpec ((=~=), Observe (..), PrintStyle (ForQuickCheck), Sig, VariableUse (Linear), bools, con, monoObserve, monoVars, quickSpec, signature, variableUse, withMaxTermSize, withMaxTestSize, withMaxTests, withPrintStyle)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck ((===), Arbitrary (..), Gen, Property, elements, frequency, property, scale, sized)

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ traverse_ (uncurry prop) quickspec_laws

printQuickCheck :: IO ()
printQuickCheck =
  quickSpec $
    sig_filters
      <> withPrintStyle ForQuickCheck
      <> withMaxTests 1000
      <> withMaxTestSize 20
      <> withMaxTermSize 7

printSmallLaws :: IO ()
printSmallLaws =
  quickSpec $
    sig_filters
      <> signature [variableUse Linear $ Proxy @(InputFilter Test)]

sig_filters :: Sig
sig_filters = sig_filter_types <> sig_filter_cons <> sig_filter_user_cons

sig_filter_types :: Sig
sig_filter_types =
  signature
    [ monoVars @(CustomFilter Test) ["f"],
      monoVars @Test ["i"],
      monoVars @Word8 ["n"],
      monoObserve @(InputFilter Test)
    ]

sig_filter_cons :: Sig
sig_filter_cons =
  signature
    [ con "always" $ always @Test,
      con "andF" $ andF @Test,
      con "never" $ never @Test,
      con "notF" $ notF @Test,
      con "orF" $ orF @Test,
      con "matches" $ matches @Test,
      bools
    ]

sig_filter_user_cons :: Sig
sig_filter_user_cons =
  signature
    [ con "exactly" exactly,
      con "Number" Number
    ]

instance Observe Test Bool (InputFilter Test) where
  observe input filter = matches filter input

instance Arbitrary (CustomFilter i) => Arbitrary (InputFilter i) where
  arbitrary = sized $ \n ->
    case n <= 1 of
      True -> elements [always, never]
      False ->
        frequency
          [ (3, pure always),
            (3, pure never),
            (5, andF <$> decayArbitrary 2 <*> decayArbitrary 2),
            (5, orF <$> decayArbitrary 2 <*> decayArbitrary 2),
            (4, notF <$> decayArbitrary 2),
            (8, custom <$> arbitrary)
          ]

newtype Test = Number Word8
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Arbitrary)

instance HasFilter Test where
  newtype CustomFilter Test = Exactly Word8
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Arbitrary)
  filterMatches (Exactly n') (Number n) = n == n'

-- instance Arbitrary Test where
--   arbitrary = Number

exactly :: Word8 -> InputFilter Test
exactly = custom . Exactly

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ( "never = notF always",
      property $ never @Test =~= notF always
    ),
    ( "always = notF never",
      property $ always @Test =~= notF never
    ),
    ( "andF x y = andF y x",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          andF x y =~= andF y x
    ),
    ( "andF x x = x",
      property $
        \(x :: InputFilter Test) -> andF x x =~= x
    ),
    ( "orF x y = orF y x",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          orF x y =~= orF y x
    ),
    ( "orF x x = x",
      property $
        \(x :: InputFilter Test) -> orF x x =~= x
    ),
    ( "matches always i",
      property $
        \(i :: Test) -> matches always i === True
    ),
    ( "matches never i = False",
      property $
        \(i :: Test) -> matches never i === False
    ),
    ( "andF x always = x",
      property $
        \(x :: InputFilter Test) -> andF x always =~= x
    ),
    ( "andF x never = never",
      property $
        \(x :: InputFilter Test) -> andF x never =~= never
    ),
    ( "orF x always = always",
      property $
        \(x :: InputFilter Test) ->
          orF x always =~= always
    ),
    ( "orF x never = x",
      property $
        \(x :: InputFilter Test) -> orF x never =~= x
    ),
    ( "notF (notF x) = x",
      property $
        \(x :: InputFilter Test) -> notF (notF x) =~= x
    ),
    ( "matches (notF x) i = not (matches x i)",
      property $
        \(i :: Test) (x :: InputFilter Test) ->
          matches (notF x) i === not (matches x i)
    ),
    ( "andF x (notF x) = never",
      property $
        \(x :: InputFilter Test) ->
          andF x (notF x) =~= never
    ),
    ( "orF x (notF x) = always",
      property $
        \(x :: InputFilter Test) ->
          orF x (notF x) =~= always
    ),
    ( "andF (andF x y) z = andF x (andF y z)",
      property $
        \(x :: InputFilter Test)
         (y :: InputFilter Test)
         (z :: InputFilter Test) ->
            andF (andF x y) z =~= andF x (andF y z)
    ),
    ( "andF x (orF x y) = x",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          andF x (orF x y) =~= x
    ),
    ( "orF x (andF x y) = x",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          orF x (andF x y) =~= x
    ),
    ( "orF (orF x y) z = orF x (orF y z)",
      property $
        \(x :: InputFilter Test)
         (y :: InputFilter Test)
         (z :: InputFilter Test) ->
            orF (orF x y) z =~= orF x (orF y z)
    ),
    ( "matches (exactly n) (Number n2) = matches (exactly n2) (Number n)",
      property $
        \(n :: Word8) (n2 :: Word8) ->
          matches (exactly n) (Number n2) === matches (exactly n2) (Number n)
    ),
    ( "matches (exactly n) (Number n)",
      property $
        \(n :: Word8) ->
          matches (exactly n) (Number n) === True
    ),
    ( "andF (notF x) (notF y) = notF (orF x y)",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          andF (notF x) (notF y) =~= notF (orF x y)
    ),
    ( "andF (notF x) (orF x y) = andF y (notF x)",
      property $
        \(x :: InputFilter Test) (y :: InputFilter Test) ->
          andF (notF x) (orF x y) =~= andF y (notF x)
    ),
    ( "matches x i && matches y i = matches (andF x y) i",
      property $
        \(i :: Test)
         (x :: InputFilter Test)
         (y :: InputFilter Test) ->
            (matches x i && matches y i) === matches (andF x y) i
    ),
    ( "matches x i || matches y i = matches (orF x y) i",
      property $
        \(i :: Test)
         (x :: InputFilter Test)
         (y :: InputFilter Test) ->
            (matches x i || matches y i) === matches (orF x y) i
    ),
    ( "andF (orF x y) (orF x z) = orF x (andF y z)",
      property $
        \(x :: InputFilter Test)
         (y :: InputFilter Test)
         (z :: InputFilter Test) ->
            andF (orF x y) (orF x z) =~= orF x (andF y z)
    ),
    ( "matches (andF x (exactly n)) (Number n2) = matches (andF x (exactly n2)) (Number n)",
      property $
        \(n :: Word8)
         (n2 :: Word8)
         (x :: InputFilter Test) ->
            matches (andF x (exactly n)) (Number n2) === matches (andF x (exactly n2)) (Number n)
    )
  ]
