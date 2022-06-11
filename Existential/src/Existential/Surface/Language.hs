{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Existential.Surface.Language where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

-- Surface language described in part 4

example :: Expr
example =
  let_ (#id, lam #x (var #x)) $
    lam #y $
      var #id ~: (forall_ #t (tyvar #t *>* tyvar #t)) ! var #y

--------------------------------------------------------------------------------
-- The Language of Types

data TypeVar where
  NamedTypeVar :: String -> TypeVar
  FreshTypeVar :: String -> Word -> TypeVar
  deriving (Eq, Ord, Show)

data UniversalType
  = Forall [TypeVar] ExistentialType
  -- The representation on the paper would actually be:
  -- = Forall TypeVar UniversalType | U ExistentialType
  -- but this is easier to work with
  deriving (Eq, Ord, Show)

data ExistentialType
  = Exists [TypeVar] TopLevelMonotype
  -- The representation on the paper would actually be:
  -- = Exists TypeVar ExistentialType | E TopLevelMonotype
  -- but this is easier to work with
  deriving (Eq, Ord, Show)

data TopLevelMonotype
  = Arrow UniversalType UniversalType
  | Top Monotype
  deriving (Eq, Ord, Show)

data Monotype
  = INT
  | Var TypeVar
  | MonoArrow Monotype Monotype
  | Projection Expr ExistentialType
  deriving (Eq, Ord, Show)

----------------------------------------
-- Sugar for Types

class ToUniversalType t where
  toUniversalType :: t -> UniversalType

instance {-# OVERLAPPING #-} ToUniversalType UniversalType where
  toUniversalType = id

instance {-# OVERLAPPABLE #-} ToExistentialType t => ToUniversalType t where
  toUniversalType = Forall [] . toExistentialType

forall_ :: ToUniversalType t => TypeVar -> t -> UniversalType
forall_ v t =
  let Forall vs exType = toUniversalType t
   in Forall (v : vs) exType

class ToExistentialType t where
  toExistentialType :: t -> ExistentialType

instance {-# OVERLAPPING #-} ToExistentialType ExistentialType where
  toExistentialType = id

instance {-# OVERLAPPABLE #-} ToTopLevelMonotype t => ToExistentialType t where
  toExistentialType = Exists [] . toTopLevelMonotype

exists_ :: ToExistentialType t => TypeVar -> t -> ExistentialType
exists_ v t =
  let Exists vs topType = toExistentialType t
   in Exists (v : vs) topType

class ToTopLevelMonotype t where
  toTopLevelMonotype :: t -> TopLevelMonotype

instance {-# OVERLAPPING #-} ToTopLevelMonotype TopLevelMonotype where
  toTopLevelMonotype = id

instance {-# OVERLAPPABLE #-} ToMonotype t => ToTopLevelMonotype t where
  toTopLevelMonotype = Top . toMonotype

(*>*) :: (ToUniversalType t1, ToUniversalType t2) => t1 -> t2 -> TopLevelMonotype
arg *>* ret = Arrow (toUniversalType arg) (toUniversalType ret)

class ToMonotype t where
  toMonotype :: t -> Monotype

instance ToMonotype Monotype where
  toMonotype = id

instance ToMonotype TypeVar where
  toMonotype = Var

(->-) :: (ToMonotype t1, ToMonotype t2) => t1 -> t2 -> Monotype
arg ->- ret = MonoArrow (toMonotype arg) (toMonotype ret)

projection :: ToExistentialType t => Expr -> t -> Monotype
projection v t = Projection v (toExistentialType t)

instance KnownSymbol s => IsLabel s TypeVar where
  fromLabel = NamedTypeVar (symbolVal @s undefined)

{- | Useful for disambiguating OverloadedLabels
 e.g. if #a is ambiguous, use `tyvar #a` which is necessarily a TypeVar
-}
tyvar :: TypeVar -> TypeVar
tyvar = id

--------------------------------------------------------------------------------
-- The Language of Expr

data TermVar = NamedTermVar String
  deriving (Eq, Ord, Show)

data Expr
  = -- Invariant: Applicable != ApplyExpr (Apply ...)
    Apply Applicable [Argument]
  | Lambda TermVar Expr
  | Let TermVar Expr Expr
  | Lit Integer
  deriving (Eq, Ord, Show)

data Argument
  = ArgExpr Expr
  | ArgType UniversalType
  deriving (Eq, Ord, Show)

data Applicable
  = ApplyVar TermVar
  | ApplyExpr Expr
  | ApplyExprTyped Expr UniversalType
  deriving (Eq, Ord, Show)

{-
Maybe this form would work to ensure the head of Apply is never an Apply?

data Expr
  = Apply Applicable (NonEmpty Argument)
  | Ex Applicable
  deriving (Eq, Ord, Show)

data Applicable
  = ApplyVar TermVar
  | Lambda TermVar Expr
  | Let TermVar Expr Expr
  | Lit Integer
  | ApplyTyped Applicable UniversalType
  deriving (Eq, Ord, Show)

or we can just add a Bool type parameter to Expr. Will try later.

-}

----------------------------------------
-- Sugar for Expr

(~:) :: (ToExpr t1, ToUniversalType t2) => t1 -> t2 -> Applicable
e ~: t = ApplyExprTyped (toExpr e) (toUniversalType t)

(!) :: (ToApplicable t1, ToArgument t2) => t1 -> t2 -> Expr
e ! arg = case toApplicable e of
  ApplyExpr expr
    | Apply h args <- expr -> Apply h (args <> [toArgument arg])
  other -> Apply (toApplicable other) [toArgument arg]

let_ :: (ToExpr t1, ToExpr t2) => (TermVar, t1) -> t2 -> Expr
let_ (boundVar, val) body = Let boundVar (toExpr val) (toExpr body)

lam :: ToExpr t => TermVar -> t -> Expr
lam argVar body = Lambda argVar (toExpr body)

class ToExpr t where
  toExpr :: t -> Expr

instance ToExpr Expr where
  toExpr = id

instance ToExpr TermVar where
  toExpr v = Apply (ApplyVar v) []

instance ToExpr Integer where
  toExpr = Lit

class ToApplicable t where
  toApplicable :: t -> Applicable

instance {-# OVERLAPPING #-} ToApplicable Applicable where
  toApplicable = id

instance {-# OVERLAPPING #-} ToApplicable TermVar where
  toApplicable = ApplyVar

instance {-# OVERLAPPABLE #-} ToExpr t => ToApplicable t where
  toApplicable = ApplyExpr . toExpr

class ToArgument t where
  toArgument :: t -> Argument

instance {-# OVERLAPPING #-} ToArgument Argument where
  toArgument = id

instance {-# OVERLAPPING #-} ToArgument UniversalType where
  toArgument = ArgType

instance {-# OVERLAPPABLE #-} ToExpr t => ToArgument t where
  toArgument = ArgExpr . toExpr

instance KnownSymbol s => IsLabel s TermVar where
  fromLabel = NamedTermVar (symbolVal @s undefined)

{- | Useful for disambiguating OverloadedLabels
 e.g. if #a is ambiguous, use `var #a` which is necessarily a TermVar
-}
var :: TermVar -> TermVar
var = id
