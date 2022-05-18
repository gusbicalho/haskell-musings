{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaPi.SimplyTyped.ByTheBook where

import Control.Monad (unless)
import Data.Data (Proxy (Proxy))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Read qualified as Read

id_ :: TermChk
id_ = Lam (Inf (Bound 0))

const_ :: TermChk
const_ = Lam (Lam (Inf (Bound 1)))

tfree :: String -> TYPE
tfree a = TFree (Global a)

free :: String -> TermChk
free x = Inf (Free (Global x))

term1 :: TermInf
term1 = id_ ~: (#a ~> #a) :@: #y

term2 :: TermInf
term2 =
  const_ ~: ((#b ~> #b) ~> #a ~> #b ~> #b)
    :@: id_
    :@: free "y"

env1 :: Context
env1 = [#y `hasType` #a, #a `hasKind` Star]

env2 :: Context
env2 = [(#b `hasKind` Star)] <> env1

{-
>>> quote0 (eval0 term1)
Inf (Free (Global "y"))

>>> quote0 (eval0 term2)
Lam (Inf (Bound 0))

>>> typeInf0 env1 term1
Right (TFree (Global "a"))

>>> typeInf0 env2 term2
Right (Fun (TFree (Global "b")) (TFree (Global "b")))
-}

-- Sugar (Not in the paper!)

hasType :: a -> TYPE -> (a, Info)
a `hasType` b = (a, HasType b)

hasKind :: a -> KIND -> (a, Info)
a `hasKind` b = (a, HasKind b)

(~:) :: TermChk -> TYPE -> TermInf
(~:) = Ann
infixl 4 ~:

(~>) :: TYPE -> TYPE -> TYPE
(~>) = Fun
infixr 5 ~>

instance KnownSymbol label => IsLabel label Name where
  fromLabel = readName $ symbolVal @label Proxy
   where
    readName ('_' : tail) = readName tail
    readName name =
      case Read.readMaybe @Word name of
        Just i -> Local i
        Nothing -> Global name

instance IsLabel label Name => IsLabel label TYPE where
  fromLabel = TFree (fromLabel @label)

instance IsLabel label Name => IsLabel label TermInf where
  fromLabel = case fromLabel @label of
    Local i -> Bound i
    name -> Free name

instance IsLabel label TermInf => IsLabel label TermChk where
  fromLabel = Inf $ fromLabel @label

-- Syntax

data TermInf
  = Ann TermChk TYPE
  | Bound Word
  | Free Name
  | TermInf :@: TermChk
  deriving (Eq, Show)
infixl 3 :@:

data TermChk
  = Inf TermInf
  | Lam TermChk
  deriving (Eq, Show)

data Name
  = Global String
  | Local Word
  | Quote Word
  deriving (Eq, Show)

data TYPE
  = TFree Name
  | Fun TYPE TYPE
  deriving (Eq, Show)

-- Values

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

vfree :: Name -> Value
vfree name = VNeutral (NFree name)

-- Evaluation

type Env = [Value]

eval0 :: TermInf -> Value
eval0 term = evalInf term []

evalInf :: TermInf -> Env -> Value
evalInf (Ann term _) env = evalChk term env
evalInf (Free name) _ = vfree name
evalInf (Bound i) env = lookupBound env i
evalInf (f :@: arg) env = vapp (evalInf f env) (evalChk arg env)

vapp :: Value -> Value -> Value
vapp (VLam run) arg' = run arg'
vapp (VNeutral n) arg' = VNeutral (NApp n arg')

evalChk :: TermChk -> Env -> Value
evalChk (Inf term) env = evalInf term env
evalChk (Lam term) env = VLam $ \arg -> evalChk term (arg : env)

lookupBound :: HasCallStack => Env -> Word -> Value
lookupBound env i = go env i
 where
  go [] _ = error $ "Unbound variable! " <> show i
  go (v : vs) i
    | i == 0 = v
    | otherwise = go vs (pred i)

-- Quoting

quote0 :: Value -> TermChk
quote0 = quote 0

quote :: Word -> Value -> TermChk
quote n = \case
  VLam run -> Lam . quote (succ n) . run $ vfree (Quote n)
  VNeutral neutral -> Inf $ quoteNeutral neutral
 where
  quoteNeutral = \case
    NFree (Quote k) -> Bound (n - k - 1)
    NFree other -> Free other
    NApp f arg -> quoteNeutral f :@: quote n arg

-- Type checking

data KIND = Star
  deriving stock (Eq, Show)

data Info
  = HasKind KIND
  | HasType TYPE
  deriving (Eq, Show)

type Context = [(Name, Info)]

type Result a = Either String a

typeInf0 :: Context -> TermInf -> Result TYPE
typeInf0 = typeInf 0

typeInf :: Word -> Context -> TermInf -> Result TYPE
typeInf i ctx = \case
  Ann term tipe -> do
    kindChk ctx tipe Star
    typeChk i ctx term tipe
    pure tipe
  Bound i ->
    throwError $ "Reference to unbound Bound: " <> show i
  Free name ->
    case lookup name ctx of
      Just (HasType tipe) -> pure tipe
      Just (HasKind _) -> throwError $ "Not a term: " <> show name
      Nothing -> throwError $ "Unknown identifier in term: " <> show name
  f :@: arg ->
    typeInf i ctx f >>= \case
      Fun argType retType -> do
        typeChk i ctx arg argType
        pure retType
      tipe -> throwError $ "Illegal application. Term " <> show f <> " is a " <> show tipe

kindChk :: Context -> TYPE -> KIND -> Result ()
kindChk ctx = go
 where
  go (TFree name) Star =
    case lookup name ctx of
      Nothing -> throwError $ "Unknown identifier in type: " <> show name
      Just (HasType _) -> throwError $ "Not a type: " <> show name
      Just (HasKind Star) -> pure ()
  go (Fun t1 t2) Star = go t1 Star *> go t2 Star

typeChk :: Word -> Context -> TermChk -> TYPE -> Result ()
typeChk i ctx = \case
  Inf term' -> \expected -> do
    inferred <- typeInf i ctx term'
    -- "Note that the type equality check that is performed when checking an
    --  inferable term is implemented by a straightforward syntactic equality on
    --  the data type Type. Our type checker does not perform unification."
    unless (expected == inferred) do
      throwError . unlines $
        [ "Type mismatch. Expected"
        , "  " <> show expected
        , "but got"
        , "  " <> show inferred
        ]
  Lam body -> \case
    Fun argType retType -> do
      typeChk
        (succ i)
        ((Local i, HasType argType) : ctx)
        (subst (Free (Local i)) 0 body)
        retType
    expected ->
      throwError . unlines $
        [ "Type mismatch. Expected"
        , "  " <> show expected
        , "but got a function."
        ]

subst :: TermInf -> Word -> TermChk -> TermChk
subst replacement = goChk
 where
  goChk i = \case
    (Inf term) -> Inf (goInf i term)
    (Lam body) -> Lam (goChk (i + 1) body)
  goInf i = \case
    Ann term tipe -> Ann (goChk i term) tipe
    Bound j
      | i == j -> replacement
      | otherwise -> Bound j
    Free y -> Free y
    f :@: arg -> goInf i f :@: goChk i arg

throwError :: String -> Result a
throwError = Left
