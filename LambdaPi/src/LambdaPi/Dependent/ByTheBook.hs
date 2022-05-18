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

plus :: TermInf
plus =
  ( Lam . Inf $ -- m
      NatElim
        (Lam . Inf $ Nat ~> Nat)
        (Lam #_0)
        ( Lam $ -- #_2 = k
            Lam $ -- #_1 = recur
              Lam $ -- #_0 = n
                suc (#_1 .@ #_0)
        )
        #_0
  )
    ~: Nat ~> Nat ~> Nat

append :: TermInf
append =
  ( Lam $ -- E
      Lam $ -- m
        Lam $ -- front :: Vec E m
          Inf $
            VecElim
              #_2
              ( Lam $ -- k
                  Lam $ -- kfront
                    Nat -- n
                      ~> Vec #_5 #_0 -- back
                      ~> Vec #_6 (plus .@ #_3 .@ #_1)
              )
              ( Lam $ -- n
                  Lam $ -- back
                    #_0
              )
              ( Lam $ -- k
                  Lam $ -- x
                    Lam $ -- xs
                      Lam $ -- recur
                        Lam $ -- n
                          Lam $ -- back
                            Inf $
                              Cons #_8 (plus .@ #_5 .@ #_1) #_4 (#_2 .@ #_1 .@ #_0)
              )
              #_1
              #_0
  )
    ~: star ~> Nat ~> Vec #_1 #_0 ~> Nat ~> Vec #_3 #_0 ~> Vec #_4 (plus .@ #_3 .@ #_1)

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

>>> quote0 . fromResult $ typeInf0 [] plus
Inf (Pi (Inf Nat) (Inf (Pi (Inf Nat) (Inf Nat))))

>>> quote0 . evalInf0 $ plus .@ nat 2 .@ nat 3
Inf (Succ (Inf (Succ (Inf (Succ (Inf (Succ (Inf (Succ (Inf Zero))))))))))

>>> front = Cons (Inf Nat) (nat 1) (nat 0) $ Inf $ Cons (Inf Nat) (nat 0) (nat 1) $ Inf $ Nil (Inf Nat)
>>> back = Cons (Inf Nat) (nat 0) (nat 2) $ Inf $ Nil (Inf Nat)
>>> x = append .@ Nat .@ nat 2 .@ front .@ nat 1 .@ back
>>> quote0 . fromResult . typeInf0 [] $ x
>>> quote0 . evalInf0 $ x
Inf (Vec (Inf Nat) (Inf (Succ (Inf (Succ (Inf (Succ (Inf Zero))))))))
Inf (Cons (Inf Nat) (Inf (Succ (Inf (Succ (Inf Zero))))) (Inf Zero) (Inf (Cons (Inf Nat) (Inf (Succ (Inf Zero))) (Inf (Succ (Inf Zero))) (Inf (Cons (Inf Nat) (Inf Zero) (Inf (Succ (Inf (Succ (Inf Zero))))) (Inf (Nil (Inf Nat))))))))

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
  nat :: Word -> term
  zero :: term
  suc :: TermInf -> term
infixr 5 ~>

instance TermSugar TermInf where
  star = Star
  x ~> t = Pi (Inf x) (Inf t)
  f .@ arg = f :@: Inf arg
  nat = \case
    0 -> Zero
    n -> Succ (nat (pred n))
  zero = Zero
  suc = Succ . Inf
infixl 3 .@

instance TermSugar TermChk where
  star = Inf star
  x ~> t = Inf (x ~> t)
  f .@ arg = Inf (f .@ arg)
  nat = Inf . nat
  zero = Inf zero
  suc = Inf . suc

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
  | -- Nats
    Nat
  | NatElim TermChk TermChk TermChk TermChk
  | Zero
  | Succ TermChk
  | -- Vectors
    Vec TermChk TermChk
  | Nil TermChk
  | Cons TermChk TermChk TermChk TermChk
  | VecElim TermChk TermChk TermChk TermChk TermChk TermChk
  deriving (Eq, Show)
infixl 3 :@:

data TermChk
  = Tag String TermChk
  | Inf TermInf
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
  | VTag String Value
  | -- Nats
    VNat
  | VZero
  | VSucc Value
  | -- Vectors
    VNil Value
  | VCons Value Value Value Value
  | VVec Value Value

data Neutral
  = NFree Name
  | NApp Neutral Value
  | -- Nats
    NNatElim Value Value Value Neutral
  | -- Vectors
    NVecElim Value Value Value Value Value Neutral

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
-- Nats
evalInf Nat _ = VNat
evalInf Zero _ = VZero
evalInf (Succ k) env = VSucc (evalChk k env)
evalInf (NatElim motive base step k) env =
  let base' = evalChk base env
      step' = evalChk step env
      go = \case
        VZero -> base'
        VSucc j -> step' `vapp` j `vapp` go j
        VNeutral k -> VNeutral $ NNatElim (evalChk motive env) base' step' k
        other -> error $ "natElim on non-Nat: " <> show (quote0 other)
   in go (evalChk k env)
-- Vectors
evalInf (Vec e size) env = VVec (evalChk e env) (evalChk size env)
evalInf (Nil e) env = VNil (evalChk e env)
evalInf (Cons e tailSize v tail) env =
  VCons
    (evalChk e env)
    (evalChk tailSize env)
    (evalChk v env)
    (evalChk tail env)
evalInf (VecElim e mot base step size xs) env =
  let e' = evalChk e env
      mot' = evalChk mot env
      base' = evalChk base env
      step' = evalChk step env
      go n = \case
        VNil _ -> base'
        VCons _ tailSize x tail ->
          step' `vapp` tailSize `vapp` x `vapp` tail `vapp` go tailSize tail
        VNeutral k -> VNeutral $ NVecElim e' mot' base' step' n k
        other -> error $ "vecElim on non-Vec: " <> show (quote0 other)
   in go (evalChk size env) (evalChk xs env)

vapp :: Value -> Value -> Value
vapp (VLam run) arg' = run arg'
vapp (VNeutral n) arg' = VNeutral (NApp n arg')
vapp (VPi _ run) arg' = run arg'
vapp (VTag _ run) arg' = vapp run arg'
vapp other _ = error $ "Not a function: " <> show (quote0 other)

evalChk :: TermChk -> Env -> Value
evalChk (Inf term) env = evalInf term env
evalChk (Lam term) env = VLam $ \arg -> evalChk term (arg : env)
evalChk (Tag s term) env = VTag s (evalChk term env)

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
quote n = quoteValue
 where
  quoteValue = \case
    VLam run -> Lam (quoteRun run)
    VNeutral neutral -> Inf $ quoteNeutral neutral
    VStar -> Inf Star
    VPi domain run -> Inf $ Pi (quoteValue domain) (quoteRun run)
    VTag tag value -> Tag tag (quoteValue value)
    -- Nats
    VNat -> Inf Nat
    VZero -> Inf Zero
    VSucc j -> Inf (Succ (quoteValue j))
    -- Vectors
    VVec e size -> Inf (Vec (quoteValue e) (quoteValue size))
    VNil e -> Inf (Nil (quoteValue e))
    VCons e tailSize x tail -> Inf (Cons (quoteValue e) (quoteValue tailSize) (quoteValue x) (quoteValue tail))
  quoteNeutral = \case
    NFree (Quote k) -> Bound (n - k - 1)
    NFree other -> Free other
    NApp f arg -> quoteNeutral f :@: quoteValue arg
    NNatElim mot base step k ->
      NatElim
        (quoteValue mot)
        (quoteValue base)
        (quoteValue step)
        (Inf (quoteNeutral k))
    NVecElim e mot base step size xs ->
      VecElim
        (quoteValue e)
        (quoteValue mot)
        (quoteValue base)
        (quoteValue step)
        (quoteValue size)
        (Inf (quoteNeutral xs))

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
  -- Nats
  Nat -> pure VStar
  Zero -> pure VNat
  Succ k -> do
    typeChk i ctx k VNat
    pure VNat
  NatElim mot base step k -> do
    typeChk i ctx mot (VPi VNat \_ -> VStar)
    let mot' = evalChk0 mot
    typeChk i ctx base $
      mot' `vapp` VZero
    typeChk i ctx step $
      VPi VNat \l ->
        VPi (mot' `vapp` l) \_ ->
          mot' `vapp` (VSucc l)
    typeChk i ctx k VNat
    pure (mot' `vapp` evalChk0 k)
  -- Vectors
  Vec e size -> do
    typeChk i ctx e VStar
    typeChk i ctx size VNat
    pure VStar
  Nil e -> do
    typeChk i ctx e VStar
    pure (VVec (evalChk0 e) VZero)
  Cons e tailSize x tail -> do
    typeChk i ctx e VStar
    let e' = evalChk0 e
    typeChk i ctx tailSize VNat
    let tailSize' = evalChk0 tailSize
    typeChk i ctx x e'
    typeChk i ctx tail (VVec e' tailSize')
    pure (VVec e' (VSucc tailSize'))
  VecElim e mot base step size xs -> do
    typeChk i ctx e VStar
    let e' = evalChk0 e
    typeChk i ctx mot (VPi VNat \size -> VPi (VVec e' size) \_ -> VStar)
    let mot' = evalChk0 mot
    typeChk i ctx base $
      mot' `vapp` VZero `vapp` VNil e'
    typeChk i ctx step $
      VPi VNat \l ->
        VPi e' \x ->
          VPi (VVec e' l) \xs ->
            VPi (mot' `vapp` l `vapp` xs) \_ ->
              mot' `vapp` VSucc l `vapp` VCons e' l x xs
    typeChk i ctx size VNat
    let size' = evalChk0 size
    typeChk i ctx xs (VVec e' size')
    let tipe = (mot' `vapp` size' `vapp` evalChk0 xs)
    pure tipe

typeChk :: Word -> Context -> TermChk -> TYPE -> Result ()
typeChk i ctx = \case
  Inf term' -> \expected -> do
    inferred <- typeInf i ctx term'
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
  Tag _ term -> typeChk i ctx term

subst :: TermInf -> Word -> TermChk -> TermChk
subst replacement = goChk
 where
  goChk i = \case
    Inf term -> Inf (goInf i term)
    Lam body -> Lam (goChk (succ i) body)
    Tag s term -> Tag s (goChk i term)
  goInf i = \case
    Ann term tipe -> Ann (goChk i term) (goChk i tipe)
    Bound j
      | i == j -> replacement
      | otherwise -> Bound j
    Free y -> Free y
    f :@: arg -> goInf i f :@: goChk i arg
    Star -> Star
    Pi domain range -> Pi (goChk i domain) (goChk (succ i) range)
    -- Nats
    Nat -> Nat
    Zero -> Zero
    Succ k -> Succ (goChk i k)
    NatElim mot base step k -> NatElim (goChk i mot) (goChk i base) (goChk i step) (goChk i k)
    -- Vectors
    Vec e size -> Vec (goChk i e) (goChk i size)
    Nil e -> Nil (goChk i e)
    Cons e tailSize x tail -> Cons (goChk i e) (goChk i tailSize) (goChk i x) (goChk i tail)
    VecElim e motive base step size xs ->
      VecElim
        (goChk i e)
        (goChk i motive)
        (goChk i base)
        (goChk i step)
        (goChk i size)
        (goChk i xs)

throwError :: String -> Result a
throwError = Left

fromResult :: Result a -> a
fromResult = \case
  Right a -> a
  Left err -> error err
