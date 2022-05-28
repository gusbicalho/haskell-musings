{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Henk where

import Control.Monad (when)
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Traversable qualified as T
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

--------------------------------------------------------------------------------
-- Syntax

data Program id lit
  = MkProgram [TypeDeclaration id lit] [ValueDeclaration id lit]
  deriving stock (Eq, Ord, Show)

data TypeDeclaration id lit
  = TypedVariable id lit ::= (NonEmpty (TypedVariable id lit))
  deriving stock (Eq, Ord, Show)

data ValueDeclaration id lit
  = Let (Binding id lit)
  | Letrec (NonEmpty (Binding id lit))
  deriving stock (Eq, Ord, Show)

data Binding id lit
  = TypedVariable id lit := Expression id lit
  deriving stock (Eq, Ord, Show)

data K = KIND | TYPE
  deriving stock (Eq, Ord, Show)

data Expression id lit
  = ELookup (TypedVariable id lit)
  | ELiteral lit
  | EK K
  | EApply (Expression id lit) (Expression id lit)
  | ELambda (TypedVariable id lit) (Expression id lit)
  | EPi (TypedVariable id lit) (Expression id lit)
  | ValueDeclaration id lit `EIn` Expression id lit
  | ECase (Expression id lit) (NonEmpty (CaseAlternative id lit)) [Expression id lit]
  deriving stock (Eq, Ord, Show)

data CaseAlternative id lit
  = CasePattern id lit :=> Expression id lit
  deriving stock (Eq, Ord, Show)

data CasePattern id lit
  = CPVariable (TypedVariable id lit)
  | CPLiteral lit
  deriving stock (Eq, Ord, Show)

data TypedVariable id lit
  = Variable id :~ Expression id lit
  deriving stock (Eq, Ord, Show)

data Variable id
  = Ignore
  | Var id
  deriving stock (Eq, Ord, Show)

-- Small piece of sugar that has to live here to avoid orphan instance
instance (KnownSymbol label, IsString id) => IsLabel label (Variable id) where
  fromLabel = case symbolVal (Proxy @label) of
    "_" -> Ignore
    other -> Var (fromString other)

--------------------------------------------------------------------------------
-- Type checking

type Result a = Either String a

type Ctx id lit = [(id, Expression id lit)]

ctxExtend :: a -> b -> [(a, b)] -> [(a, b)]
ctxExtend id exp ctx = (id, exp) : ctx

kAxioms :: K -> Maybe K
kAxioms = \case
  TYPE -> Just KIND
  KIND -> Nothing

(~~>) :: K -> K -> Maybe K
KIND ~~> t = Just t -- Types can be bound in Terms or in Types
TYPE ~~> TYPE = Just TYPE -- Terms can be bound in Terms
TYPE ~~> KIND = Nothing -- Terms CANNOT be bound in Types

checkProgram ::
  (Eq id, Eq lit, Show id, Show lit) =>
  (lit -> Expression id lit) ->
  Ctx id lit ->
  Program id lit ->
  Result (Ctx id lit)
checkProgram typeOfLiteral baseCtx (MkProgram typedecls valuedecls) =
  pure baseCtx
    >>= checkTypes
    >>= checkValues
 where
  checkTypes ctx =
    F.foldlM
      (extendingCtx (checkTypeDeclaration typeOfLiteral))
      ctx
      typedecls
  checkValues ctx =
    F.foldlM
      (extendingCtx (checkValueDeclaration typeOfLiteral))
      ctx
      valuedecls
  extendingCtx f accum v = (<> accum) <$> f accum v

checkTypeDeclaration ::
  forall id lit.
  (Eq id, Eq lit, Show id, Show lit) =>
  (lit -> Expression id lit) ->
  Ctx id lit ->
  TypeDeclaration id lit ->
  Result (Ctx id lit)
checkTypeDeclaration typeOfLiteral ctx typedecl@(typevar ::= constructors) = do
  typeCtxEntry@(_, typeKind) <- typedVarToCtxEntry typevar
  checkTypeOrKind ctx typeKind
  constructorsCtx <- traverse typedVarToCtxEntry constructors
  let ctxWithType = typeCtxEntry : ctx
  F.for_ constructorsCtx \(_, constructorKind) -> do
    checkTypeOrKind ctxWithType constructorKind
  pure $ F.toList constructorsCtx <> [typeCtxEntry]
 where
  -- Check if tipe is EType or EKind
  checkTypeOrKind :: Ctx id lit -> Expression id lit -> Result ()
  checkTypeOrKind ctx tipe =
    checkTypeExpression typeOfLiteral ctx tipe >>= \case
      EK _ -> pure ()
      other ->
        Left $
          "Expected\n  "
            <> show tipe
            <> "\n to be a Type or Kind, but instead got\n  "
            <> show other
  typedVarToCtxEntry (name :~ tipe) =
    case name of
      Ignore -> Left $ "Top-level declarations must have an identifier, missing at:\n" <> show typedecl
      Var identifier -> pure (identifier, tipe)

checkValueDeclaration ::
  (Eq id, Eq lit, Show id, Show lit) =>
  (lit -> Expression id lit) ->
  Ctx id lit ->
  ValueDeclaration id lit ->
  Result (Ctx id lit)
checkValueDeclaration typeOfLiteral ctx = \case
  Let binding -> List.singleton <$> checkSingleBinding typeOfLiteral ctx binding
  Letrec bindings -> do
    let newCtxEntries = [(name, tipe) | (Var name :~ tipe := _) <- F.toList bindings]
    let innerCtx = newCtxEntries <> ctx
    F.toList <$> traverse (checkSingleBinding typeOfLiteral innerCtx) bindings

checkSingleBinding ::
  (Show id, Show lit, Eq id, Eq lit) =>
  (lit -> Expression id lit) ->
  Ctx id lit ->
  Binding id lit ->
  Either String (id, Expression id lit)
checkSingleBinding typeOfLiteral ctx binding@(var :~ tipe := expr) = do
  varId <- case var of
    Ignore -> Left $ "Found binding declaration with no name. Useless!\n  " <> show binding
    Var name -> pure name
  inferred <- checkTypeExpression typeOfLiteral ctx expr
  when (tipe /= inferred) do
    Left $
      "Expected an expression of type\n  "
        <> show tipe
        <> "\nbut inferred type\n  "
        <> show inferred
        <> "\nfor expression\n  "
        <> show expr
  pure (varId, tipe)

checkTypeExpression ::
  (Eq id, Eq lit, Show id, Show lit) =>
  (lit -> Expression id lit) ->
  Ctx id lit ->
  Expression id lit ->
  Result (Expression id lit)
checkTypeExpression typeOfLiteral = go
 where
  go ctx = \case
    ELiteral lit -> pure $ typeOfLiteral lit
    EK k -> case kAxioms k of
      Just s -> pure (EK s)
      Nothing -> Left $ show k <> " does not have a type"
    ELookup (Ignore :~ _) -> Left "Cannot lookup variable _"
    ELookup (Var var :~ expected) -> case lookup var ctx of
      Nothing -> Left $ "Free var " <> show var
      Just bound -> do
        when (expected /= bound) do
          Left $
            "Expected variable " <> show var <> " to have type\n  "
              <> show expected
              <> "\nbut it is bound to type\n  "
              <> show bound
        pure bound
    EApply apply argument -> do
      apply_type <- betaReduce <$> go ctx apply
      case apply_type of
        EPi (boundVar :~ bindingType) boundExpr -> do
          argument_type <- betaReduce <$> go ctx argument
          when (not $ isWHNF argument_type) $
            Left $ "Unable to reduce argument type to WHNF:\n  " <> show argument_type
          when (bindingType /= argument_type) $
            Left $
              "Bad argument. Expected a\n  "
                <> show bindingType
                <> "\nbut got\n  "
                <> show argument
                <> "\nwhich is a\n  "
                <> show argument_type
          pure case boundVar of
            Ignore -> boundExpr
            Var boundId -> subst boundId argument boundExpr
        other -> Left $ "Non-PI type in application: " <> show other
    ELambda (boundVar :~ bindingType) boundExpr -> do
      let extendedCtx = case boundVar of
            Ignore -> ctx
            Var boundName -> ctxExtend boundName bindingType ctx
      return_type <- go extendedCtx boundExpr
      let pi_type =
            EPi (boundVar :~ bindingType) return_type
      -- check that the Pi type is well sorted
      _ <- go ctx pi_type
      pure pi_type
    EPi (boundVar :~ bindingType) boundExpr -> do
      bindingType_kind <-
        betaReduce <$> go ctx bindingType >>= \case
          EK k -> pure k
          other -> Left $ "Unable to find Pi binding type kind in WHNF:\n  " <> show other
      let extendedCtx = case boundVar of
            Ignore -> ctx
            Var boundName -> ctxExtend boundName bindingType ctx
      resultType_kind <-
        betaReduce <$> go extendedCtx boundExpr >>= \case
          EK k -> pure k
          other -> Left $ "Unable to find Pi result type kind in WHNF:\n  " <> show other
      case bindingType_kind ~~> resultType_kind of
        Just piType_type -> pure (EK piType_type)
        Nothing ->
          Left $
            "Pi binding not allowed: "
              <> (show bindingType <> " : " <> show bindingType_kind)
              <> " ~~> "
              <> (show boundExpr <> " : " <> show resultType_kind)
    localDecl `EIn` body -> do
      localCtx <- checkValueDeclaration typeOfLiteral ctx localDecl
      tipe <- go (localCtx <> ctx) body
      tipe_kind <-
        betaReduce <$> go ctx tipe >>= \case
          EK k -> pure k
          other -> Left $ "Unable to find body type kind in WHNF:\n  " <> show other
      F.for_ localCtx \(_, typeOfLocallyBoundName) -> do
        typeOfLocallyBoundName_kind <-
          betaReduce <$> go ctx typeOfLocallyBoundName >>= \case
            EK k -> pure k
            other -> Left $ "Unable to find local binding type kind in WHNF:\n  " <> show other
        case typeOfLocallyBoundName_kind ~~> tipe_kind of
          Just piType_type -> pure (EK piType_type)
          Nothing ->
            Left $
              "Let binding not allowed: "
                <> (show typeOfLocallyBoundName <> " : " <> show typeOfLocallyBoundName_kind)
                <> " ~~> "
                <> (show tipe <> " : " <> show tipe_kind)
      pure tipe
    ECase scrutinee cases atClause -> do
      scrutinee_type <- go ctx scrutinee
      cases_types <- T.for cases \(pat :=> handler) -> do
        let pat_raw_type = case pat of
              CPLiteral lit -> typeOfLiteral lit
              CPVariable (_ :~ t) -> t
        pat_type <- go ctx (F.foldl' EApply pat_raw_type atClause)
        handler_type <- go ctx handler
        (pat :=>) <$> checkCaseAlternativeType scrutinee_type pat_type handler_type
      pure $ ECase scrutinee cases_types atClause
  checkCaseAlternativeType scrutinee_type pat_type handler_type
    | scrutinee_type == pat_type =
        -- Here we finished traversing the bindings of pattern and handler types,
        -- until we got to the end: the point where we find the type of the
        -- scrutinee. Since we've matched everything so far, the handler works.
        pure handler_type
    | EPi (_ :~ pat_binding_type) pat_bound <- pat_type
    , EPi (_ :~ handler_binding_type) handler_bound <- handler_type
    , pat_binding_type == handler_binding_type =
        -- If the pattern has a Pi type, the handler must also have a Pi
        -- type, and the types of bindings in these Pi types must match.
        -- If that works all the way down, we have a type that will take
        -- the right number of arguments and, in the end, return the type of
        -- the handler.
        EPi (Ignore :~ pat_binding_type) <$> checkCaseAlternativeType scrutinee_type pat_bound handler_bound
    | otherwise =
        Left $
          "Type mismatch in case alternative. Pattern has type\n  "
            <> show pat_type
            <> "\nbut case handler has type\n  "
            <> show handler_type

isWHNF :: Expression id lit -> Bool
isWHNF EApply{} = False
isWHNF _ = True

betaReduce :: Eq id => Expression id lit -> Expression id lit
betaReduce = \case
  e@(ELiteral _) -> e
  e@(EK _) -> e
  e@ELambda{} -> e
  e@EPi{} -> e
  e@ELookup{} -> e
  e@EIn{} -> e
  e@ECase{} -> e
  EApply apply argument ->
    let apply' = betaReduce apply
        argument' = betaReduce argument
     in case apply' of
          ELambda (Var var :~ _) body -> betaReduce $ subst var argument' body
          ELambda (Ignore :~ _) body -> betaReduce body
          EPi (Var var :~ _) body -> betaReduce $ subst var argument' body
          EPi (Ignore :~ _) body -> betaReduce body
          _ -> EApply apply' argument'

subst :: Eq id => id -> Expression id lit -> Expression id lit -> Expression id lit
subst var value = go
 where
  go = \case
    e@(ELiteral _) -> e
    e@(EK _) -> e
    e@(ELookup v)
      | (Var vname :~ _) <- v, vname == var -> value
      | otherwise -> e
    EPi (v :~ t) body ->
      EPi (v :~ go t) (if v == Var var then body else go body)
    ELambda (v :~ t) body ->
      ELambda (v :~ go t) (if v == Var var then body else go body)
    EApply apply argument -> EApply (go apply) (go argument)
    valuedecl `EIn` body ->
      case valuedecl of
        Let binding
          | var `isShadowedBy` [binding] ->
              -- shadowing happens, so we must not subst in the body
              -- but since this Let is not recursive
              -- we still have to subst in the binding
              Let (goBinding binding) `EIn` body
          | otherwise ->
              Let (goBinding binding) `EIn` go body
        Letrec bindings
          | var `isShadowedBy` bindings ->
              -- since this is a recursive let, the shadowing is complete
              Letrec bindings `EIn` body
          | otherwise ->
              -- no shadowing, so just recur everywhere
              Letrec (goBinding <$> bindings) `EIn` go body
    ECase scrutinee cases atClause ->
      ECase
        (go scrutinee)
        (goCaseAlternative <$> cases)
        (go <$> atClause)
  goBinding (localVar :~ localType := bindingValue) =
    localVar :~ go localType := go bindingValue
  goCaseAlternative (pat :=> expr) = pat :=> go expr

{-# INLINE isShadowedBy #-}
isShadowedBy :: (Eq a, Foldable t) => a -> t (Binding a lit) -> Bool
varId `isShadowedBy` bindings =
  varId `elem` foldMap (F.toList . declaredName) bindings

declaredNames :: ValueDeclaration id lit -> [id]
declaredNames = \case
  Let binding -> F.toList $ declaredName binding
  Letrec bindings -> foldMap (F.toList . declaredName) bindings

declaredName :: Binding id lit -> Maybe id
declaredName (Ignore :~ _ := _) = Nothing
declaredName (Var name :~ _ := _) = Just name
