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
  Eval,
  eval,
  eval0,
  vapp,
  Quote (..),
  quote0,
  checkType,
  inferType,
  inferType0,
  subst,
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

-- Evaluation

type Env ext = [(Value ext)]

class Extension ext => Eval term ext | term -> ext where
  eval :: term -> Env ext -> Value ext

eval0 :: Eval term ext => term -> Value ext
eval0 term = eval term []

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

-- Quoting

class Quote value term | value -> term where
  quote :: Word -> value -> term

quote0 :: Quote value term => value -> term
quote0 = quote 0

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

-- Type checking

type TYPE ext = Value ext

type Context ext = [(Name, TYPE ext)]

type Result a = Either String a

inferType0 :: Extension ext => Context ext -> TermInf ext -> Result (TYPE ext)
inferType0 = inferType 0

inferType :: Extension ext => Word -> (Context ext) -> (TermInf ext) -> Result (TYPE ext)
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
  Ext ext -> typeExt i ctx ext

checkType :: Extension ext => Word -> (Context ext) -> (TermChk ext) -> (TYPE ext) -> Result ()
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

subst :: Extension ext => (TermInf ext) -> Word -> (TermChk ext) -> (TermChk ext)
subst replacement = goChk
 where
  goChk i = \case
    Inf term -> Inf (goInf i term)
    Lam body -> Lam (goChk (succ i) body)
  goInf i = \case
    Ann term tipe -> Ann (goChk i term) (goChk i tipe)
    Bound j
      | i == j -> replacement
      | otherwise -> Bound j
    Free y -> Free y
    f :@: arg -> goInf i f :@: goChk i arg
    Star -> Star
    Pi domain range -> Pi (goChk i domain) (goChk (succ i) range)
    Ext ext -> Ext (substExt replacement i ext)

throwError :: String -> Result a
throwError = Left

-- Extension

type Extension a = TypeExtension a a

type TypeExtension :: Type -> Type -> Constraint
class
  ( Eq (ExtTerm ext extSet)
  , Show (ExtTerm ext extSet)
  , Extension extSet
  , Includes (ExtTerm extSet extSet) (ExtTerm ext extSet)
  , Includes (ExtNeutral extSet extSet) (ExtNeutral ext extSet)
  , Includes (ExtValue extSet extSet) (ExtValue ext extSet)
  , Quote (ExtValue ext extSet) (ExtTerm ext extSet)
  , Quote (ExtNeutral ext extSet) (ExtTerm ext extSet)
  , Eval (ExtTerm ext extSet) extSet
  ) =>
  TypeExtension ext extSet
  where
  type ExtTerm ext extSet = t | t -> ext extSet
  type ExtValue ext extSet = t | t -> ext extSet
  type ExtNeutral ext extSet = t | t -> ext extSet
  typeExt :: Word -> Context extSet -> ExtTerm ext extSet -> Result (Value extSet)
  substExt :: TermInf extSet -> Word -> ExtTerm ext extSet -> ExtTerm ext extSet

data Empty extSet
  deriving (Eq, Show)

instance (Void ~ extSet) => TypeExtension Void extSet where
  type ExtTerm Void extSet = Empty extSet
  type ExtValue Void extSet = Empty extSet
  type ExtNeutral Void extSet = Empty extSet
  typeExt _ _ = \case {}
  substExt _ _ = \case {}

instance Quote (Empty Void) (Empty Void) where
  quote _ = \case {}

instance Eval (Empty Void) Void where
  eval t _ = case t of {}

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
  typeExt n ctx = either (typeExt n ctx) (typeExt n ctx)
  substExt r n = either (Left . substExt r n) (Right . substExt r n)

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
