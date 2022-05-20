{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

{- | Code here based on the on code from the paper,  but refactor to allow
 flexibly adding extensions
-}
module LambdaPi.Dependent.MyExtensions.Core (
  TermInf (..),
  TermChk (..),
  Value (..),
  Neutral (..),
  Extension,
  TypeExtension (..),
  Includes (..),
  Env,
  Context,
  Result,
  Eval (..),
  eval0,
  vapp,
  Quote (..),
  quote0,
  checkType,
  InferType (..),
  inferType0,
  Subst (..),
  TermSugar (..),
  hasType,
  (~:),
) where

import Control.Monad (unless)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import LambdaPi.Dependent.MyExtensions.Includes (Includes)
import LambdaPi.Dependent.MyExtensions.Includes qualified as Includes
import Text.Read qualified as Read
import Prelude hiding (pi)

-- Syntax

data TermInf ext
  = Ann (TermChk ext) (TermChk ext)
  | Star
  | Pi (TermChk ext) (TermChk ext)
  | Bound Word
  | Free Name
  | (TermInf ext) :@: (TermChk ext)
  | Ext (ExtTerm ext ext)

infixl 3 :@:

deriving stock instance Eq (ExtTerm ext ext) => Eq (TermInf ext)
deriving stock instance Show (ExtTerm ext ext) => Show (TermInf ext)

data TermChk ext
  = Inf (TermInf ext)
  | Lam (TermChk ext)

deriving stock instance Eq (TermInf ext) => Eq (TermChk ext)
deriving stock instance Show (TermInf ext) => Show (TermChk ext)

data Name
  = Global String
  | Local Word
  | Quote Word
  deriving (Eq, Show)

-- Sugar

hasType :: Extension ext => a -> (TermInf ext) -> (a, (Value ext))
a `hasType` b = (a, eval0 b)

(~:) :: (TermChk ext) -> (TermChk ext) -> (TermInf ext)
(~:) = Ann
infixl 4 ~:

class TermSugar term extSet where
  star :: term extSet
  (~>) :: TermInf extSet -> TermInf extSet -> term extSet
  (.@) :: TermInf extSet -> TermInf extSet -> term extSet
  ext :: TypeExtension ext extSet => ExtTerm ext extSet -> term extSet

infixr 5 ~>
infixl 3 .@

instance TermSugar TermInf ext where
  star = Star
  x ~> t = Pi (Inf x) (Inf t)
  f .@ arg = f :@: Inf arg
  ext t = Ext (Includes.inject t)

instance TermSugar TermChk ext where
  star = Inf star
  x ~> t = Inf (x ~> t)
  f .@ arg = Inf (f .@ arg)
  ext = Inf . ext

instance KnownSymbol label => IsLabel label Name where
  fromLabel = readName $ symbolVal @label Proxy
   where
    readName ('_' : tail) = readName tail
    readName name =
      case Read.readMaybe @Word name of
        Just i -> Local i
        Nothing -> Global name

instance IsLabel label Name => IsLabel label (TermInf ext) where
  fromLabel = case fromLabel @label of
    Local i -> Bound i
    name -> Free name

instance IsLabel label (TermInf ext) => IsLabel label (TermChk ext) where
  fromLabel = Inf $ fromLabel @label

-- Values

data Value ext
  = VLam ((Value ext) -> (Value ext))
  | VStar
  | VPi (Value ext) ((Value ext) -> (Value ext))
  | VNeutral (Neutral ext)
  | VExt (ExtValue ext ext)

data Neutral ext
  = NFree Name
  | NApp (Neutral ext) (Value ext)
  | NExt (ExtNeutral ext ext)

vfree :: Name -> (Value ext)
vfree name = VNeutral (NFree name)

-- Extension classes

type Extension a = TypeExtension a a

{- | A TypeExtension instance has two type parameters:
  * `ext`: The first parameter is a type that names the extension being
    implemented. For example, the LambdaPi.Dependent.MyExtensions.NatExt
    type names the extension of the calculus with Nats.
  * `extSet`: The second parameter is the name of the set of extensions
    used in some specific usage of the calculus. Concretely, this could
    be:
      - the name of a single extension, such as `NatExt`
      - the name of a sum of extensions, such as `Either NatExt VecExt`

A TypeExtension instance may add constraints to the `extSet` parameter,
for example if it cannot work unless another extension is also included.
Some examples:
  * `instance TypeExtension NatExt extSet where`
    implements an extension named `NatExt` that can be used alongside any other
    extension.
  * `instance (TypeExtension NatExt extSet) => TypeExtension VecExt extSet`
    implements an extension named `VecExt` that requires that the `NatExt`
    extension also be included in the calculus.
  * `instance TypeExtension Foo Foo`
    implements an extension named `Foo` that call only be used alone, never
    alongside other extensions (because it fixes extSet = Foo).

In any case, the `extSet` must be a valid and complete TypeExtension, in the
sense that it can be used on its own. This restriction is implemented by the
first superclass constraint: `TypeExtension extSet extSet`.
For comparison, the `VecExt` extension is not complete in this sense, since it
requires the NatExt extension. Therefore, there is no instance for
`TypeExtension VecExt VecExt`.

# `Includes` superclasses:

The `Includes` class represents the ability to inject a datatype into a larger
sum type, or to project from a sum type into one of its components. This ability
is necessary when building and operating on Terms and Values, so that datatypes
that belong to one specific extension can be mixed with datatypes from other
extensions, all in a single big sum type.

For example, the constraint
`Includes (ExtTerm extSet extSet) (ExtTerm ext extSet)` means that the language
of Terms of the extension `ext` can be included in the language of Terms of
`extSet`.

As a more concrete example, the following instance exists:
```
  Includes
    (ExtTerm (Either NatExt VecExt) (Either NatExt VecExt))
    (ExtTerm NatExt (Either NatExt VecExt))
```
If we simplify the `ExtTerm` type family applications, we get the following:
```
  Includes
    (Either
      (TermNat (Either NatExt VecExt))
      (TermVec (Either NatExt VecExt)))
    (TermNat (Either NatExt VecExt))
```
Which tells us that values of type `TermNat ...` can be injected into the sum
`Either (TermNat ...) (TermVec ...)`.
-}
type TypeExtension :: Type -> Type -> Constraint
class
  ( TypeExtension extSet extSet
  , Includes (ExtTerm extSet extSet) (ExtTerm ext extSet)
  , Includes (ExtNeutral extSet extSet) (ExtNeutral ext extSet)
  , Includes (ExtValue extSet extSet) (ExtValue ext extSet)
  , Eq (ExtTerm ext extSet)
  , Show (ExtTerm ext extSet)
  , Eval (ExtTerm ext extSet) extSet
  , InferType (ExtTerm ext extSet) extSet
  , Subst (ExtTerm ext extSet) extSet
  , Quote (ExtValue ext extSet) (ExtTerm ext extSet)
  , Quote (ExtNeutral ext extSet) (ExtTerm ext extSet)
  ) =>
  TypeExtension ext extSet
  where
  type ExtTerm ext extSet = t | t -> ext extSet
  type ExtValue ext extSet = t | t -> ext extSet
  type ExtNeutral ext extSet = t | t -> ext extSet

type Env ext = [(Value ext)]

class Extension ext => Eval term ext | term -> ext where
  eval :: term -> Env ext -> Value ext

eval0 :: Eval term ext => term -> Value ext
eval0 term = eval term []

class Quote value term | value -> term where
  quote :: Word -> value -> term

quote0 :: Quote value term => value -> term
quote0 = quote 0

type Context ext = [(Name, Value ext)]

type Result a = Either String a

class InferType term ext | term -> ext where
  inferType :: Word -> (Context ext) -> term -> Result (Value ext)

inferType0 :: Extension ext => Context ext -> TermInf ext -> Result (Value ext)
inferType0 = inferType 0

throwError :: String -> Result a
throwError = Left

class Subst term ext | term -> ext where
  subst :: Extension ext => TermInf ext -> Word -> term -> term

-- Core evaluation

instance Extension ext => Eval (TermInf ext) ext where
  eval term env = case term of
    (Ann term _) -> eval term env
    Star -> VStar
    (Pi domain range) ->
      VPi (eval domain env) (\arg -> eval range (arg : env))
    (Free name) -> vfree name
    (Bound i) -> lookupBound env i
    (f :@: arg) -> vapp (eval f env) (eval arg env)
    (Ext ext) -> eval ext env

instance Extension ext => Eval (TermChk ext) ext where
  eval term env = case term of
    (Inf term) -> eval term env
    (Lam term) -> VLam $ \arg -> eval term (arg : env)

vapp :: (HasCallStack, Extension ext) => Value ext -> Value ext -> Value ext
vapp (VLam run) arg' = run arg'
vapp (VNeutral n) arg' = VNeutral (NApp n arg')
vapp (VPi _ run) arg' = run arg'
vapp other _ = error $ "Not a function: " <> show (quote0 other)

lookupBound :: HasCallStack => Env ext -> Word -> (Value ext)
lookupBound env i = go env i
 where
  go [] _ = error $ "Unbound variable! " <> show i
  go (v : vs) i
    | i == 0 = v
    | otherwise = go vs (pred i)

-- Core quoting

instance Extension ext => Quote (Value ext) (TermChk ext) where
  quote n = go
   where
    go = \case
      VLam run -> Lam (quoteRun run)
      VNeutral neutral -> Inf $ quote n neutral
      VStar -> Inf Star
      VPi domain run -> Inf $ Pi (go domain) (quoteRun run)
      VExt ext -> Inf (Ext (quote n ext))
    quoteRun run = quote (succ n) (run $ vfree (Quote n))

instance Extension ext => Quote (Neutral ext) (TermInf ext) where
  quote n = go
   where
    go = \case
      NFree (Quote k) -> Bound (n - k - 1)
      NFree other -> Free other
      NApp f arg -> go f :@: quote n arg
      NExt neutralNat -> Ext (quote n neutralNat)

-- Core type checking

instance Extension ext => InferType (TermInf ext) ext where
  inferType i ctx = \case
    -- If the term is well formed, we must never reach Bound forms - they
    -- must all have been subst'd with Free (Local x).
    Bound i ->
      throwError $ "Reference to unbound Bound: " <> show i
    -- "Note that we assume that the term under consideration in inferType has no
    --  unbound variables, so all calls to evalChk take an empty environment."
    -- The Free (Local i) created by subst will be evaluated into (Neutral ext) values,
    -- and later be read back via quote, so that the type equality test will work.
    Ann term tipe -> do
      checkType i ctx tipe VStar
      let tipe' = eval0 tipe
      checkType i ctx term tipe'
      pure tipe'
    Star -> pure VStar
    Pi domain range -> do
      checkType i ctx domain VStar
      let domain' = eval0 domain
      checkType
        (succ i)
        ((Local i, domain') : ctx)
        (subst (Free (Local i)) 0 range)
        VStar
      pure VStar
    Free name ->
      case lookup name ctx of
        Just tipe -> pure tipe
        Nothing -> throwError $ "Unknown identifier in term: " <> show name
    f :@: arg ->
      inferType i ctx f >>= \case
        VPi domain range -> do
          checkType i ctx arg domain
          pure (range (eval0 arg))
        tipe ->
          throwError $
            unwords
              [ "Illegal application. Term"
              , show f
              , "is a"
              , show (quote0 tipe)
              ]
    Ext ext -> inferType i ctx ext

checkType :: Extension ext => Word -> (Context ext) -> (TermChk ext) -> (Value ext) -> Result ()
checkType i ctx = \case
  Inf term' -> \expected -> do
    inferred <- inferType i ctx term'
    let expected' = quote0 expected
    let inferred' = quote0 inferred
    unless (expected' == inferred') do
      throwError . unlines $
        [ "Type mismatch on term"
        , "  " <> show term'
        , "Expected"
        , "  " <> show expected'
        , "but got"
        , "  " <> show inferred'
        ]
  Lam body -> \case
    VPi domain range -> do
      checkType
        (succ i)
        ((Local i, domain) : ctx)
        (subst (Free (Local i)) 0 body)
        (range (vfree (Local i)))
    expected ->
      throwError . unlines $
        [ "Type mismatch. Expected"
        , "  " <> show (quote0 expected)
        , "but got a function."
        ]

-- Core subst

instance Subst (TermChk ext) ext where
  subst replacement = go
   where
    go i = \case
      Lam body -> Lam (go (succ i) body)
      Inf term -> Inf (subst @(TermInf ext) replacement i term)

instance Subst (TermInf ext) ext where
  subst replacement = go
   where
    go i = \case
      Ann term tipe -> Ann (goChk i term) (goChk i tipe)
      Bound j
        | i == j -> replacement
        | otherwise -> Bound j
      Free y -> Free y
      f :@: arg -> go i f :@: goChk i arg
      Star -> Star
      Pi domain range -> Pi (goChk i domain) (goChk (succ i) range)
      Ext ext -> Ext (subst replacement i ext)
    goChk i = subst @(TermChk ext) replacement i

-- Helper extension instances

-- | Empty: No entensions, only core calculus
data Empty extSet
  deriving (Eq, Show)

instance
  ( Extension extSet
  , Includes (ExtTerm extSet extSet) (Empty extSet)
  , Includes (ExtValue extSet extSet) (Empty extSet)
  , Includes (ExtNeutral extSet extSet) (Empty extSet)
  ) =>
  TypeExtension Void extSet
  where
  type ExtTerm Void extSet = Empty extSet
  type ExtValue Void extSet = Empty extSet
  type ExtNeutral Void extSet = Empty extSet

instance Includes whatever (Empty ext) where
  inject = \case {}
  project _ = Nothing
instance Quote (Empty extSet) (Empty extSet) where
  quote _ = \case {}

instance Extension extSet => Eval (Empty extSet) extSet where
  eval t _ = case t of {}

instance Extension extSet => InferType (Empty extSet) extSet where
  inferType _ _ = \case {}

instance Subst (Empty extSet) extSet where
  subst _ _ = \case {}

-- | Either extension - use two extensions together
instance
  ( TypeExtension a extSet
  , TypeExtension b extSet
  , Includes (ExtTerm extSet extSet) (ExtTerm (Either a b) extSet)
  , Includes (ExtNeutral extSet extSet) (ExtNeutral (Either a b) extSet)
  , Includes (ExtValue extSet extSet) (ExtValue (Either a b) extSet)
  , Quote (ExtValue (Either a b) extSet) (ExtTerm (Either a b) extSet)
  , Quote (ExtNeutral (Either a b) extSet) (ExtTerm (Either a b) extSet)
  ) =>
  TypeExtension (Either a b) extSet
  where
  type ExtTerm (Either a b) extSet = Either (ExtTerm a extSet) (ExtTerm b extSet)
  type ExtValue (Either a b) extSet = Either (ExtValue a extSet) (ExtValue b extSet)
  type ExtNeutral (Either a b) extSet = Either (ExtNeutral a extSet) (ExtNeutral b extSet)

instance
  ( (Either aTerm bTerm) ~ ExtTerm (Either a b) extSet
  , Extension extSet
  , TypeExtension a extSet
  , TypeExtension b extSet
  ) =>
  Eval (Either aTerm bTerm) extSet
  where
  eval = either eval eval

instance
  ( (Either aTerm bTerm) ~ ExtTerm (Either a b) extSet
  , Extension extSet
  , TypeExtension a extSet
  , TypeExtension b extSet
  ) =>
  InferType (Either aTerm bTerm) extSet
  where
  inferType n ctx = either (inferType n ctx) (inferType n ctx)

instance
  ( (Either aTerm bTerm) ~ ExtTerm (Either a b) extSet
  , Extension extSet
  , TypeExtension a extSet
  , TypeExtension b extSet
  ) =>
  Subst (Either aTerm bTerm) extSet
  where
  subst r n = either (Left . subst r n) (Right . subst r n)

instance
  ( TypeExtension a extSet
  , TypeExtension b extSet
  , aTerm ~ ExtTerm a extSet
  , bTerm ~ ExtTerm b extSet
  , QuoteDispatch a (TyEq aVal (ExtNeutral a extSet)) aVal aTerm
  , QuoteDispatch b (TyEq bVal (ExtNeutral b extSet)) bVal bTerm
  ) =>
  Quote
    (Either aVal bVal)
    (Either aTerm bTerm)
  where
  quote n = either (Left . quote @aVal n) (Right . quote @bVal n)

type TyEq :: Type -> Type -> Bool
type family TyEq a b where
  TyEq a a = 'True
  TyEq _ _ = 'False

class (Quote value term) => QuoteDispatch ext isNeutral value term | value -> ext term
instance
  ( TypeExtension ext extSet
  , value ~ ExtValue ext extSet
  , term ~ ExtTerm ext extSet
  , Quote value term
  ) =>
  QuoteDispatch ext 'False value term
instance
  ( value ~ ExtNeutral ext extSet
  , term ~ ExtTerm ext extSet
  , TypeExtension ext extSet
  , Quote value term
  ) =>
  QuoteDispatch ext 'True value term
