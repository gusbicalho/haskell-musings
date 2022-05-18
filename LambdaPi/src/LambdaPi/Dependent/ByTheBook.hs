{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaPi.Dependent.ByTheBook where

import Control.Monad (unless)
import Data.Data (Proxy (Proxy))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Read qualified as Read
import Prelude hiding (pi)

id_ :: TermInf
id_ = Lam (Lam #_0) ~: star ~> #_0 ~> #_1

const_ :: TermInf
const_ = Lam (Lam (Lam (Lam #_1))) ~: star ~> star ~> #_1 ~> #_1 ~> #_3

term1 :: TermInf
term1 = id_ :@: #a :@: #y

term2 :: TermInf
term2 =
  const_
    .@ #b ~> #b
    .@ #a
    .@ (id_ .@ #b)
    .@ #y

env1 :: Context
env1 = [#y `hasType` #a, #a `hasType` Star]

env2 :: Context
env2 = [(#b `hasType` Star)] <> env1

{-
>>> quote0 . fromResult . typeInf0 [] $ id_
Inf (Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 1)))))

>>> quote0 . fromResult . typeInf0 [] $ const_
Inf (Pi (Inf Star) (Inf (Pi (Inf Star) (Inf (Pi (Inf (Bound 1)) (Inf (Pi (Inf (Bound 1)) (Inf (Bound 3)))))))))

>>> quote0 (evalInf0 term1)
Inf (Free (Global "y"))

>>> quote0 (evalInf0 term2)
Lam (Inf (Bound 0))

>>> quote0 . fromResult $ typeInf0 env1 term1
Inf (Free (Global "a"))

>>> quote0 . fromResult $ typeInf0 env2 term2
Inf (Pi (Inf (Free (Global "b"))) (Inf (Free (Global "b"))))
-}

-- Sugar (Not in the paper!)

hasType :: a -> TermInf -> (a, Value)
a `hasType` b = (a, evalInf0 b)

(~:) :: TermChk -> TermChk -> TermInf
(~:) = Ann
infixl 4 ~:

class TermSugar term where
  star :: term
  (~>) :: TermInf -> TermInf -> term
  (.@) :: TermInf -> TermInf -> term
infixr 5 ~>

instance TermSugar TermInf where
  star = Star
  x ~> t = Pi (Inf x) (Inf t)
  f .@ arg = f :@: Inf arg
infixl 3 .@

instance TermSugar TermChk where
  star = Inf star
  x ~> t = Inf (x ~> t)
  f .@ arg = Inf (f .@ arg)

instance KnownSymbol label => IsLabel label Name where
  fromLabel = readName $ symbolVal @label Proxy
   where
    readName ('_' : tail) = readName tail
    readName name =
      case Read.readMaybe @Word name of
        Just i -> Local i
        Nothing -> Global name

instance IsLabel label Name => IsLabel label TermInf where
  fromLabel = case fromLabel @label of
    Local i -> Bound i
    name -> Free name

instance IsLabel label TermInf => IsLabel label TermChk where
  fromLabel = Inf $ fromLabel @label

-- Syntax

data TermInf
  = Ann TermChk TermChk
  | Star
  | Pi TermChk TermChk
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

-- Values

data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

vfree :: Name -> Value
vfree name = VNeutral (NFree name)

-- Evaluation

type Env = [Value]

evalInf0 :: TermInf -> Value
evalInf0 term = evalInf term []

evalChk0 :: TermChk -> Value
evalChk0 term = evalChk term []

evalInf :: TermInf -> Env -> Value
evalInf (Ann term _) env = evalChk term env
evalInf Star _ = VStar
evalInf (Pi domain range) env =
  VPi (evalChk domain env) (\arg -> evalChk range (arg : env))
evalInf (Free name) _ = vfree name
evalInf (Bound i) env = lookupBound env i
evalInf (f :@: arg) env = vapp (evalInf f env) (evalChk arg env)

vapp :: Value -> Value -> Value
vapp (VLam run) arg' = run arg'
vapp (VNeutral n) arg' = VNeutral (NApp n arg')
vapp (VPi _ run) arg' = run arg'
vapp VStar _ = error "VStar is not a function!"

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
  VLam run -> Lam (quoteRun run)
  VNeutral neutral -> Inf $ quoteNeutral neutral
  VStar -> Inf Star
  VPi domain run -> Inf $ Pi (quote n domain) (quoteRun run)
 where
  quoteNeutral = \case
    NFree (Quote k) -> Bound (n - k - 1)
    NFree other -> Free other
    NApp f arg -> quoteNeutral f :@: quote n arg
  quoteRun run = quote (succ n) (run $ vfree (Quote n))

-- Type checking

type TYPE = Value

type Context = [(Name, TYPE)]

type Result a = Either String a

typeInf0 :: Context -> TermInf -> Result TYPE
typeInf0 = typeInf 0

typeInf :: Word -> Context -> TermInf -> Result TYPE
typeInf i ctx = \case
  -- If the term is well formed, we must never reach Bound forms - they
  -- must all have been subst'd with Free (Local x).
  Bound i ->
    throwError $ "Reference to unbound Bound: " <> show i
  -- "Note that we assume that the term under consideration in typeInf has no
  --  unbound variables, so all calls to evalChk take an empty environment."
  -- The Free (Local i) created by subst will be evaluated into Neutral values,
  -- and later be read back via quote, so that the type equality test will work.
  Ann term tipe -> do
    typeChk i ctx tipe VStar
    let tipe' = evalChk0 tipe
    typeChk i ctx term tipe'
    pure tipe'
  Star -> pure VStar
  Pi domain range -> do
    typeChk i ctx domain VStar
    let domain' = evalChk0 domain
    typeChk
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
    typeInf i ctx f >>= \case
      VPi domain range -> do
        typeChk i ctx arg domain
        pure (range (evalChk0 arg))
      tipe ->
        throwError $
          unwords
            [ "Illegal application. Term"
            , show f
            , "is a"
            , show (quote0 tipe)
            ]

typeChk :: Word -> Context -> TermChk -> TYPE -> Result ()
typeChk i ctx = \case
  Inf term' -> \expected -> do
    inferred <- typeInf i ctx term'
    let expected' = quote0 expected
    let inferred' = quote0 inferred
    unless (expected' == inferred') do
      throwError . unlines $
        [ "Type mismatch. Expected"
        , "  " <> show expected'
        , "but got"
        , "  " <> show inferred'
        ]
  Lam body -> \case
    VPi domain range -> do
      typeChk
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

subst :: TermInf -> Word -> TermChk -> TermChk
subst replacement = goChk
 where
  goChk i = \case
    (Inf term) -> Inf (goInf i term)
    (Lam body) -> Lam (goChk (succ i) body)
  goInf i = \case
    Ann term tipe -> Ann (goChk i term) (goChk i tipe)
    Bound j
      | i == j -> replacement
      | otherwise -> Bound j
    Free y -> Free y
    f :@: arg -> goInf i f :@: goChk i arg
    Star -> Star
    Pi domain range -> Pi (goChk i domain) (goChk (succ i) range)

throwError :: String -> Result a
throwError = Left

fromResult :: Result a -> a
fromResult = \case
  Right a -> a
  Left err -> error err
