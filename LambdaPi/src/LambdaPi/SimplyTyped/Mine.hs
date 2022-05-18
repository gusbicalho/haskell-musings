{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module LambdaPi.SimplyTyped.Mine where

import Data.Data (Proxy (Proxy))
import Data.Foldable qualified as F
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

-- Implementation mostly based on content up to section 2.3
-- Plus I took a hint about higher-order abstract syntax from the beginning
-- of 2.4, and the hint about "quoting" inspired the implementation of Show Value

e :: EXPR
e = ("f", (#a :~> #a)) :-> ("z", #a) :-> #f $. #f :. #z

-- >>> e
-- "f" :-> ("z" :-> Lookup "f" :. (Lookup "f" :. Lookup "z"))

id_ :: EXPR
id_ = ("x", #a) :-> #x

const_ :: EXPR
const_ = ("x", #a) :-> ("y", #b) :-> #x

{-
>>> e
("f",BASE "a" :~> BASE "a") :-> (("z",BASE "a") :-> Lookup "f" :. (Lookup "f" :. Lookup "z"))

>>> typecheck [("a", IsType)] e
Right ((BASE "a" :~> BASE "a") :~> (BASE "a" :~> BASE "a"))

>>> eval e
( \f -> ( \z -> ( #f ( #f #z ) ) ) )

>>> id_
("x",BASE "a") :-> Lookup "x"

>>> typecheck [("a", IsType)] id_
Right (BASE "a" :~> BASE "a")

>>> eval id_
( \x -> #x )

>>> eval $ id_ :. #bar
#bar

>>> const_
("x",BASE "a") :-> (("y",BASE "b") :-> Lookup "x")

>>> typecheck [("a", IsType), ("b", IsType)] const_
Right (BASE "a" :~> (BASE "b" :~> BASE "a"))

>>> eval const_
( \x -> ( \y -> #x ) )

>>> eval $  (const_ $.. [#bar, #foo])
#bar

-}

-- Syntax

data TYPE
  = BASE String
  | !TYPE :~> !TYPE
  deriving stock (Eq, Ord, Show)

instance KnownSymbol varName => IsLabel varName TYPE where
  fromLabel = BASE (symbolVal @varName Proxy)

data EXPR
  = EXPR :~ TYPE
  | Lookup String
  | EXPR :. EXPR
  | (String, TYPE) :-> EXPR
  deriving stock (Eq, Ord, Show)
infixr 1 :->
infixl 3 :.

($.) :: EXPR -> EXPR -> EXPR
($.) = (:.)
infixr 2 $.

($..) :: EXPR -> [EXPR] -> EXPR
($..) = F.foldl' (:.)
infixr 2 $..

instance KnownSymbol varName => IsLabel varName EXPR where
  fromLabel = Lookup (symbolVal @varName Proxy)

-- Values

data Value
  = Neutral NeutralTerm
  | Lambda String (Value -> Value)

data NeutralTerm
  = Variable String
  | Application NeutralTerm Value

instance Show Value where
  show (Lambda argName run) =
    unwords
      [ "("
      , "\\" <> argName
      , "->"
      , show . run $ Neutral (Variable argName)
      , ")"
      ]
  show (Neutral n) = showNeutral n
   where
    showNeutral (Variable s) = "#" <> s
    showNeutral (Application n v) = unwords ["(", showNeutral n, show v, ")"]

-- Evaluation

eval :: EXPR -> Value
eval = \case
  e :~ _ -> eval e
  Lookup x -> Neutral (Variable x)
  (name, _) :-> body ->
    let body' = eval body
     in Lambda name \arg -> bindIn name arg body'
  f :. arg -> case eval f of
    Neutral f' -> Neutral $ Application f' (eval arg)
    Lambda _ run -> run (eval arg)

bindIn :: String -> Value -> Value -> Value
bindIn name arg = go
 where
  go = \case
    Neutral neutral -> bindNeutral neutral
    Lambda argName f -> Lambda argName \v -> go (f v)
  bindNeutral = \case
    v@(Variable name')
      | name == name' -> arg
      | otherwise -> Neutral v
    Application f v ->
      case (bindNeutral f, go v) of
        (Neutral f', v') -> Neutral (Application f' v')
        (Lambda _ f', v') -> f' v'

-- Type checking

data Binding
  = IsType
  | HasType TYPE
  deriving stock (Eq, Ord, Show)

type Context = [(String, Binding)]

data ContextError
  = UnknownBaseType String
  | ExpectedTypeButNameRefersToTerm String
  | BadBinding String Binding ContextError
  deriving stock (Eq, Ord, Show)

badBindings :: Context -> [ContextError]
badBindings [] = []
badBindings ((name, binding) : more) =
  ( case binding of
      IsType -> []
      HasType t -> BadBinding name binding <$> checkIsTypeIn more t
  )
    <> badBindings more

checkIsTypeIn :: Context -> TYPE -> [ContextError]
checkIsTypeIn ctx =
  let go = \case
        BASE name -> case lookup name ctx of
          Nothing -> [UnknownBaseType name]
          Just (HasType _) -> [ExpectedTypeButNameRefersToTerm name]
          Just (IsType) -> []
        t1 :~> t2 -> go t1 <> go t2
   in go

data TYPEError
  = BadContext (NonEmpty ContextError)
  | TypeMismatch {expected :: TYPE, actual :: TYPE}
  | ExpectedTermButNameRefersToType String
  | UnknownVariableName String
  | NonFunctionInApplication TYPE
  deriving stock (Eq, Ord, Show)

checkContext :: Context -> Either TYPEError ()
checkContext ctx = case NonEmpty.nonEmpty (badBindings ctx) of
  Nothing -> pure ()
  Just bad -> Left (BadContext bad)

typecheck :: Context -> EXPR -> Either TYPEError TYPE
typecheck ctx = \expr -> do
  checkContext ctx
  infer expr
 where
  infer :: EXPR -> Either TYPEError TYPE
  infer expr =
    case expr of
      expr' :~ annType ->
        check expr' annType $> annType
      Lookup varName ->
        case lookup varName ctx of
          Just (HasType t) -> pure t
          Just IsType -> Left (ExpectedTermButNameRefersToType varName)
          Nothing -> Left (UnknownVariableName varName)
      f :. arg ->
        infer f >>= \case
          argType :~> retType -> do
            check arg argType
            pure retType
          other -> Left $ NonFunctionInApplication other
      (argName, argType) :-> body -> do
        retType <- typecheck ((argName, HasType argType) : ctx) body
        pure (argType :~> retType)
  check :: EXPR -> TYPE -> Either TYPEError ()
  check expr expected = do
    inferred <- infer expr
    checkSameType expected inferred
  checkSameType :: TYPE -> TYPE -> Either TYPEError ()
  checkSameType expected actual = case (expected, actual) of
    (BASE a, BASE b)
      | a /= b -> Left $ TypeMismatch (BASE a) (BASE b)
      | otherwise -> pure ()
    (a :~> b, c :~> d) -> checkSameType a c *> checkSameType b d
    _ -> Left $ TypeMismatch expected actual
