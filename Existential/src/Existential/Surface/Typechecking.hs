{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Existential.Surface.Typechecking where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (unless)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (Except, ExceptT)
import Control.Monad.Trans.Except qualified as ExceptT
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple qualified as Tuple
import Existential.Fresh (FreshT)
import Existential.Fresh qualified as Fresh
import Existential.ReportTypeErrors qualified as ReportTypeErrors
import Existential.Surface.Language
import Existential.Surface.Unification (UniVar, Unification)
import Existential.Surface.Unification qualified as Unification

example :: Expr
example =
  let_ (#id, lam #x (var #x)) $
    lam #y $
      var #id ~: (forall_ #t (tyvar #t *>* tyvar #t)) ! var #y

--------------------------------------------------------------------------------
-- Effects

type TC = FreshT (ExceptT String Unification)

runTC :: TC a -> Either String a
runTC = Unification.runUnification . ExceptT.runExceptT . Fresh.runFreshT

freshTypeVar :: String -> TC TypeVar
freshTypeVar tag = FreshTypeVar tag <$> Fresh.fresh

--------------------------------------------------------------------------------
-- Typing context

data ContextEntry
  = Skolem TypeVar
  | Unifiable TypeVar UniVar
  | HasType TermVar UniversalType

type Context = [ContextEntry]

lookupTermVarType :: TermVar -> Context -> Maybe UniversalType
lookupTermVarType target = go
 where
  go [] = Nothing
  go (entry : more)
    | HasType v t <- entry
    , v == target =
        Just t
    | otherwise = go more

knownTypeVars :: Context -> Set TypeVar
knownTypeVars = F.foldl' go Set.empty
 where
  go acc = \case
    HasType _ _ -> acc
    Skolem tvar -> Set.insert tvar acc
    Unifiable tvar _ -> Set.insert tvar acc

--------------------------------------------------------------------------------
-- Universal type checking

typeCheckUniversal :: Context -> Expr -> UniversalType -> TC ()
typeCheckUniversal ctx expr (Forall univVars (Exists exVars exType)) = do
  let skolems = map Skolem univVars
  let univCtx = skolems <> ctx
  existentials <- traverse (\ex -> (ex,) <$> Unification.freshUniVar) exVars
  typeCheck (fmap (uncurry Unifiable) existentials <> univCtx) expr exType
  inferredMonos <- Trans.lift $ Unification.freezeAllWithDefault INT (snd <$> existentials)
  F.for_ inferredMonos (checkNoUnboundTypeVariables ctx)
  pure ()

--------------------------------------------------------------------------------
-- Synthesizing

typeSynth :: Context -> Expr -> TC TopLevelMonotype
typeSynth ctx = synthExpr
 where
  synthExpr :: Expr -> TC TopLevelMonotype
  synthExpr = \case
    -- Rule: Int
    Lit _ -> pure $ Top INT
    -- Rule: IAbs
    Lambda param body -> do
      guessVar <- freshTypeVar "IAbs"
      guessUni <- Unification.freshUniVar
      let innerCtx = [HasType param (toUniversalType guessVar), Unifiable guessVar guessUni] <> ctx
      synthesizedResultType <- typeSynth innerCtx body
      resultType <-
        toUniversalType <$> replaceAllProjectionsThatMentionsVarWithFreshTypeVariable param synthesizedResultType
      argType <- Trans.lift $ Unification.freezeWithDefault INT guessUni
      pure (Arrow (toUniversalType argType) resultType)
    -- Rule: Let
    Let bindingVar boundValue body -> _
    -- Rule: App
    Apply head args -> do
      headType <- headSynth ctx head
      _
  replaceAllProjectionsThatMentionsVarWithFreshTypeVariable ::
    TermVar -> TopLevelMonotype -> TC ExistentialType
  replaceAllProjectionsThatMentionsVarWithFreshTypeVariable target = \originalType -> do
    (innerType, generatedBindings) <- flip StateT.runStateT Map.empty $ goReplaceTop originalType
    pure $ Exists (Map.elems generatedBindings) innerType
   where
    goReplaceTop (Arrow argType retType) = Arrow <$> (goReplaceU argType) <*> (goReplaceU retType)
    goReplaceTop (Top mono) = Top <$> goReplaceMono mono
    goReplaceU (Forall bindings existType) = Forall bindings <$> goReplaceE existType
    goReplaceE (Exists bindings topType) = Exists bindings <$> goReplaceTop topType
    goReplaceMono (MonoArrow arg ret) = MonoArrow <$> goReplaceMono arg <*> goReplaceMono ret
    goReplaceMono p@(Projection e t)
      | target `appearsFreeIn` p = do
          bindings <- StateT.get
          case Map.lookup (e, t) bindings of
            Just tvar -> pure (Var tvar)
            Nothing -> do
              freshVar <- Trans.lift $ freshTypeVar "synthInnerExistential"
              StateT.modify' (Map.insert (e, t) freshVar)
              pure (Var freshVar)
    goReplaceMono mono = pure mono

typeCheck :: Context -> Expr -> TopLevelMonotype -> TC ()
typeCheck ctx = checkExpr
 where
  checkExpr :: Expr -> TopLevelMonotype -> TC ()
  -- Rule: CAbs
  checkExpr (Lambda param body) (Arrow argType retType) = do
    checkNoUnboundTypeVariables ctx argType
    typeCheckUniversal (HasType param argType : ctx) body retType
  -- Rules: Int, App
  checkExpr expr tipe = do
    synthesizedType <- typeSynth ctx expr
    unless (synthesizedType == tipe) $ do
      ReportTypeErrors.typeError
        [ "Expected type"
        , ReportTypeErrors.ind1 tipe
        , "but expression"
        , ReportTypeErrors.ind1 expr
        , "has type"
        , ReportTypeErrors.ind1 synthesizedType
        ]

headSynth :: Context -> Applicable -> TC UniversalType
headSynth ctx = \case
  -- Rule: H-Var
  ApplyVar v -> case lookupTermVarType v ctx of
    Just t -> pure t
    Nothing ->
      ReportTypeErrors.typeError
        [ "Applying unknown variable " <> show v
        ]
  -- Rule: H-Ann
  ApplyExprTyped e t -> do
    typeCheckUniversal ctx e t
    checkNoUnboundTypeVariables ctx t
    pure t
  -- Rule: H-Infer
  ApplyExpr e -> toUniversalType <$> typeSynth ctx e

instantiate ::
  Context ->
  Expr ->
  UniversalType ->
  [Argument] ->
  [UniversalType] ->
  TC TopLevelMonotype
instantiate = \ctx apHead headType arguments argumentTypes ->
  goForall ctx apHead headType =<< zipArgsWithTypes arguments argumentTypes
 where
  zipArgsWithTypes :: [Argument] -> [UniversalType] -> TC [Either UniversalType (Expr, UniversalType)]
  zipArgsWithTypes [] [] = pure []
  zipArgsWithTypes [] argTypes@(_ : _) =
    ReportTypeErrors.typeError
      [ "Implementation error: too many types provided"
      , ReportTypeErrors.ind (show <$> argTypes)
      ]
  zipArgsWithTypes (ArgExpr e : _) [] =
    ReportTypeErrors.typeError
      [ "Implementation error: missing type for argument"
      , ReportTypeErrors.ind1 e
      ]
  zipArgsWithTypes (ArgType t : args) argTypes =
    (Left t :) <$> zipArgsWithTypes args argTypes
  zipArgsWithTypes (ArgExpr e : args) (t : argTypes) =
    (Right (e, t) :) <$> zipArgsWithTypes args argTypes
  goForall :: Context -> Expr -> UniversalType -> [Either UniversalType (Expr, UniversalType)] -> TC TopLevelMonotype
  goForall ctx apHead headType arguments =
    case headType of
      Forall [] existType -> goExist ctx apHead existType arguments
      Forall (univBinding : moreUs) existType
        -- ITyArg
        | Left typeArg : moreArgs <- arguments -> do
            checkNoUnboundTypeVariables ctx typeArg
            goForall
              ctx
              (apHead ! ArgType typeArg)
              (substTypeVarUniversal univBinding typeArg (Forall moreUs existType))
              moreArgs
        -- IAll
        | otherwise -> do
            guess <- Unification.freshUniVar
            let innerCtx = Unifiable univBinding guess : ctx
            result <- goForall innerCtx apHead (Forall moreUs existType) arguments
            do
              -- check that no free vars were involved in the inference
              inferredMonos <- Trans.lift $ Unification.freezeAllWithDefault INT [guess]
              F.traverse_ (checkNoUnboundTypeVariables ctx) inferredMonos
            pure result
  goExist :: Context -> Expr -> ExistentialType -> [Either UniversalType (Expr, UniversalType)] -> TC TopLevelMonotype
  goExist ctx apHead headType arguments =
    case headType of
      Exists [] topType -> goTop ctx apHead topType arguments
      -- IExist
      Exists (exBinding : moreEs) topType -> do
        goExist
          ctx
          apHead
          ( substTypeVarMono
              exBinding
              (Projection apHead headType)
              (Exists moreEs topType)
          )
          arguments
  goTop :: Context -> Expr -> TopLevelMonotype -> [Either UniversalType (Expr, UniversalType)] -> TC TopLevelMonotype
  goTop ctx apHead topType arguments =
    case topType of
      -- IArg
      Arrow argExpectedType retType
        | (Right (argExpr, argType) : moreArgs) <- arguments
        , argExpectedType == argType ->
            goForall ctx (apHead ! argExpr) retType moreArgs
      -- IResult
      _ | [] <- arguments -> pure topType
      _ ->
        ReportTypeErrors.typeError
          [ "Unexpected arguments"
          , ReportTypeErrors.ind (show <$> arguments)
          , "provided to expression of type"
          , ReportTypeErrors.ind1 topType
          ]

-- Helpers
class SubstTypeVar t where
  substTypeVarMono :: TypeVar -> Monotype -> t -> t
  substTypeVarUniversal :: TypeVar -> UniversalType -> t -> UniversalType

instance SubstTypeVar UniversalType where
  substTypeVarMono tvar replacement t@(Forall bindings existType)
    -- shadowing
    | tvar `elem` bindings = t
    -- no shadowing
    | otherwise = Forall bindings (substTypeVarMono tvar replacement existType)
  substTypeVarUniversal tvar replacement t@(Forall bindings existType)
    -- shadowing
    | tvar `elem` bindings = t
    -- no shadowing
    | otherwise = case substTypeVarUniversal tvar replacement existType of
        Forall innerBindings innerType -> Forall (bindings <> innerBindings) innerType

instance SubstTypeVar ExistentialType where
  substTypeVarMono tvar replacement t@(Exists bindings topType)
    -- shadowing
    | tvar `elem` bindings = t
    -- no shadowing
    | otherwise = Exists bindings (substTypeVarMono tvar replacement topType)
  substTypeVarUniversal tvar replacement t@(Exists bindings existType)
    -- shadowing
    | tvar `elem` bindings = Forall [] t
    -- no shadowing
    | otherwise = case substTypeVarUniversal tvar replacement existType of
        Forall univBindings (Exists innerBindings innerType) ->
          Forall univBindings (Exists (bindings <> innerBindings) innerType)

instance SubstTypeVar TopLevelMonotype where
  substTypeVarMono tvar replacement = \case
    Arrow argType retType ->
      Arrow (substTypeVarMono tvar replacement argType) (substTypeVarMono tvar replacement retType)
    Top mono -> Top (substTypeVarMono tvar replacement mono)
  substTypeVarUniversal tvar replacement = \case
    Arrow argType retType ->
      toUniversalType $
        Arrow
          (substTypeVarUniversal tvar replacement argType)
          (substTypeVarUniversal tvar replacement retType)
    Top mono -> substTypeVarUniversal tvar replacement mono

instance SubstTypeVar Monotype where
  substTypeVarMono tvar replacement = \case
    Var v
      | v == tvar -> replacement
      | otherwise -> Var v
    MonoArrow argType retType ->
      MonoArrow (substTypeVarMono tvar replacement argType) (substTypeVarMono tvar replacement retType)
    INT -> INT
    p@(Projection{}) -> p
  substTypeVarUniversal tvar replacement = \case
    Var v
      | v == tvar -> replacement
      | otherwise -> toUniversalType $ Var v
    MonoArrow argType retType ->
      toUniversalType $
        Arrow (substTypeVarUniversal tvar replacement argType) (substTypeVarUniversal tvar replacement retType)
    INT -> toUniversalType INT
    p@(Projection{}) -> toUniversalType p

checkNoUnboundTypeVariables :: (CollectFreeTypeVariables t, Show t) => Context -> t -> TC ()
checkNoUnboundTypeVariables ctx t =
  let knownVars = knownTypeVars ctx
      freeVars = freeTypeVariables t
      unboundVars = Set.difference freeVars knownVars
   in if Set.null unboundVars
        then pure ()
        else
          ReportTypeErrors.typeError
            [ "Unknown type variables"
            , ReportTypeErrors.ind (show <$> F.toList unboundVars)
            , "found in type"
            , ReportTypeErrors.ind1 t
            ]

class CollectFreeTypeVariables t where
  freeTypeVariables :: t -> Set TypeVar

instance CollectFreeTypeVariables UniversalType where
  freeTypeVariables (Forall bindings univType) =
    F.foldl' (flip Set.delete) (freeTypeVariables univType) bindings

instance CollectFreeTypeVariables ExistentialType where
  freeTypeVariables (Exists bindings existType) =
    F.foldl' (flip Set.delete) (freeTypeVariables existType) bindings

instance CollectFreeTypeVariables TopLevelMonotype where
  freeTypeVariables = \case
    Arrow arg ret -> freeTypeVariables arg <> freeTypeVariables ret
    Top mono -> freeTypeVariables mono

instance CollectFreeTypeVariables Monotype where
  freeTypeVariables = \case
    Var t -> Set.singleton t
    MonoArrow arg ret -> freeTypeVariables arg <> freeTypeVariables ret
    _ -> Set.empty

class AppearsFreeIn t where
  appearsFreeIn :: TermVar -> t -> Bool

instance AppearsFreeIn UniversalType where
  appearsFreeIn target (Forall _bindings univType) =
    appearsFreeIn target univType

instance AppearsFreeIn ExistentialType where
  appearsFreeIn target (Exists _bindings existType) =
    appearsFreeIn target existType

--   F.foldl' (flip Set.delete) (appearsFreeIn existType) bindings

instance AppearsFreeIn TopLevelMonotype where
  appearsFreeIn target = \case
    Arrow arg ret -> appearsFreeIn target arg || appearsFreeIn target ret
    Top mono -> appearsFreeIn target mono

instance AppearsFreeIn Monotype where
  appearsFreeIn target = \case
    Projection e t -> appearsFreeIn target e || appearsFreeIn target t
    _ -> False

instance AppearsFreeIn Expr where
  appearsFreeIn target = \case
    Apply h args -> appearsFreeIn target h || F.any (appearsFreeIn target) args
    Lit _ -> False
    Lambda param body
      -- shadowing
      | param == target -> False
      -- no shadowing
      | otherwise -> appearsFreeIn target body
    Let name bound body
      -- shadowing
      | name == target -> appearsFreeIn target bound
      -- no shadowing
      | otherwise -> appearsFreeIn target bound || appearsFreeIn target body

instance AppearsFreeIn Applicable where
  appearsFreeIn target = \case
    ApplyVar _ -> False
    ApplyExpr e -> appearsFreeIn target e
    ApplyExprTyped e t -> appearsFreeIn target e || appearsFreeIn target t

instance AppearsFreeIn Argument where
  appearsFreeIn target = \case
    ArgExpr e -> appearsFreeIn target e
    ArgType t -> appearsFreeIn target t
