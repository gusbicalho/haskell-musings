{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Bidirectional.Context where

import Bidirectional.Language
import Bidirectional.ReportTypeErrors
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set

--------------------------------------------------------------------------------
-- Contexts

data CtxBinding
  = HasType Tipe
  | IsUniversal
  | IsExistential (Maybe Mono)
  deriving (Eq, Ord, Show)

data ExistentialMarker = MkExistentialMarker Var
  deriving (Eq, Ord, Show)

data Ctx = MkCtx
  { ctxBindings :: [Either ExistentialMarker (Var, CtxBinding)]
  , ctxDomain :: Set Var
  , ctxMarkers :: Set ExistentialMarker
  }
  deriving (Eq, Ord, Show)

emptyCtx :: Ctx
emptyCtx = MkCtx [] mempty mempty

--------------------------------------------------------------------------------
-- Monotypes - solutions to existentials

data Mono
  = MonoUnit
  | MonoVar Var
  | MonoFunction Mono Mono
  deriving stock (Eq, Ord, Show)

monoToTipe :: Mono -> Tipe
monoToTipe = \case
  MonoUnit -> TUnit
  MonoVar v -> TVar v
  MonoFunction arg ret -> TFunction (monoToTipe arg) (monoToTipe ret)

--------------------------------------------------------------------------------
-- Queries

lookupBinding :: Var -> Ctx -> Maybe CtxBinding
lookupBinding var MkCtx{ctxBindings, ctxDomain}
  | var `Set.notMember` ctxDomain = Nothing
  | otherwise = Maybe.listToMaybe . Maybe.mapMaybe check $ ctxBindings
 where
  check (Left _) = Nothing
  check (Right (v, binding))
    | var == v = Just binding
    | otherwise = Nothing

varIsBoundToAType :: ReportTypeErrors m => Ctx -> Var -> m ()
varIsBoundToAType ctx var =
  case lookupBinding var ctx of
    Just IsUniversal -> pure ()
    Just IsExistential{} -> pure ()
    Just (HasType termType) ->
      typeError
        [ "Expected a type, but"
        , ind [show var]
        , "is a term of type"
        , ind [show termType]
        ]
    Nothing ->
      typeError ["Unbound var (3)", ind [show var]]

varIsBoundToATerm :: ReportTypeErrors m => Ctx -> Var -> m Tipe
varIsBoundToATerm ctx var =
  case lookupBinding var ctx of
    Just (HasType termType) -> pure termType
    Just IsUniversal -> errorBoundToType
    Just IsExistential{} -> errorBoundToType
    Nothing ->
      typeError ["Unbound var (4)", ind [show var]]
 where
  errorBoundToType =
    typeError
      [ "Expected a term, but"
      , ind [show var]
      , "is a Type."
      ]

{- | Assuming both vars are defined, tells us whether the first Var wqas defined
 before the second Var
-}
definedBefore :: Ctx -> Var -> Var -> Bool
definedBefore MkCtx{ctxBindings} varBefore varAfter =
  foldr go False ctxBindings
 where
  go (Left _) acc = acc
  go (Right (v, _)) acc
    -- The varAfter is more to the right, thus it was defined later
    -- so varBefore was definedBefore the varAfter
    | v == varAfter = True
    -- The varBefore is more to the right, thus it was defined later
    -- so varBefore was definitely not definedBefore the varAfter
    | v == varBefore = False
    -- We didn't find either var yet, so we keep going
    | otherwise = acc

unsolvedExistentials :: Ctx -> [Var]
unsolvedExistentials MkCtx{ctxBindings} =
  ctxBindings & Maybe.mapMaybe \case
    Right (v, IsExistential Nothing) -> Just v
    _ -> Nothing

dropEntriesUntilBinding ::
  Var ->
  Ctx ->
  (Ctx, Maybe CtxBinding, [Either ExistentialMarker (Var, CtxBinding)])
dropEntriesUntilBinding target = go []
 where
  go dropped ctx@MkCtx{ctxBindings = []} =
    (ctx, Nothing, dropped)
  go dropped ctx@MkCtx{ctxBindings = entry@(Left marker) : more, ctxMarkers} =
    let cleanCtx = ctx{ctxBindings = more, ctxMarkers = Set.delete marker ctxMarkers}
     in go (entry : dropped) cleanCtx
  go dropped ctx@MkCtx{ctxBindings = entry@(Right (var, binding)) : more, ctxDomain} =
    let cleanCtx = ctx{ctxBindings = more, ctxDomain = Set.delete var ctxDomain}
     in if var == target
          then (cleanCtx, Just binding, dropped)
          else go (entry : dropped) cleanCtx

dropEntriesUntilBinding_ :: Var -> Ctx -> Ctx
dropEntriesUntilBinding_ target = (\(ctx, _, _) -> ctx) . dropEntriesUntilBinding target

dropEntriesUntilMarkerOf :: Var -> Ctx -> (Ctx, Maybe ExistentialMarker, [Either ExistentialMarker (Var, CtxBinding)])
dropEntriesUntilMarkerOf target = go []
 where
  go dropped ctx@MkCtx{ctxBindings = []} = (ctx, Nothing, dropped)
  go dropped ctx@MkCtx{ctxBindings = entry@(Right (var, _)) : more, ctxDomain} =
    go (entry : dropped) ctx{ctxBindings = more, ctxDomain = Set.delete var ctxDomain}
  go dropped ctx@MkCtx{ctxBindings = entry@(Left marker@(MkExistentialMarker var)) : more, ctxMarkers} =
    let cleanCtx = ctx{ctxBindings = more, ctxMarkers = Set.delete marker ctxMarkers}
     in if var == target
          then (cleanCtx, Just marker, dropped)
          else go (entry : dropped) cleanCtx

dropEntriesUntilMarkerOf_ :: Var -> Ctx -> Ctx
dropEntriesUntilMarkerOf_ target = go
 where
  go ctx@MkCtx{ctxBindings = []} = ctx
  go ctx@MkCtx{ctxBindings = Right (var, _) : more, ctxDomain} =
    go ctx{ctxBindings = more, ctxDomain = Set.delete var ctxDomain}
  go ctx@MkCtx{ctxBindings = Left marker@(MkExistentialMarker var) : more, ctxMarkers} =
    let cleanCtx = ctx{ctxBindings = more, ctxMarkers = Set.delete marker ctxMarkers}
     in if var == target
          then cleanCtx
          else go cleanCtx

--------------------------------------------------------------------------------
-- Application of contexts

defaultAllUnsolvedExistentials :: ReportTypeErrors m => Mono -> Ctx -> m Ctx
defaultAllUnsolvedExistentials solution ctx =
  F.foldlM
    (\ctx' var -> solveExistential var solution ctx')
    ctx
    (unsolvedExistentials ctx)

substCtxInType :: Ctx -> Tipe -> Tipe
substCtxInType ctx = go
 where
  go TUnit = TUnit
  go t@(TVar var) = case lookupBinding var ctx of
    Just (IsExistential (Just solved)) -> go (monoToTipe solved)
    _ -> t
  go (TFunction argType retType) = TFunction (go argType) (go retType)
  go (TForall universalName tipe) = TForall universalName (go tipe)

--------------------------------------------------------------------------------
-- Well-formedness of types and monotypes

isWellFormedMono :: ReportTypeErrors m => Ctx -> Mono -> m ()
isWellFormedMono ctx = \case
  -- UnitWF
  MonoUnit -> pure ()
  -- UvarWF, EvarWF, SolvedEvarWF
  (MonoVar var) -> varIsBoundToAType ctx var
  -- ArrowWF
  (MonoFunction argType retType) -> do
    isWellFormedMono ctx argType
    isWellFormedMono ctx retType

isWellFormedType :: ReportTypeErrors m => Ctx -> Tipe -> m ()
isWellFormedType ctx = \case
  -- UnitWF
  TUnit -> pure ()
  -- UvarWF, EvarWF, SolvedEvarWF
  (TVar var) -> varIsBoundToAType ctx var
  -- ArrowWF
  (TFunction argType retType) -> do
    isWellFormedType ctx argType
    isWellFormedType ctx retType
  -- ForallWF
  TForall forallVar univType -> do
    extendedCtx <- bindUniversal forallVar ctx
    isWellFormedType extendedCtx univType

doesNotOccurFreeIn :: ReportTypeErrors m => Var -> Tipe -> m ()
doesNotOccurFreeIn target fullType = go fullType
 where
  go (TVar v)
    | v == target =
        typeError
          [ "Variable"
          , ind1 target
          , "is not free in type"
          , ind1 fullType
          ]
    | otherwise = pure ()
  go (TForall forallVar univType)
    -- shadowing
    | forallVar == target = pure ()
    -- no shadowing
    | otherwise = go univType
  go TUnit = pure ()
  go (TFunction argType retType) = go argType *> go retType

--------------------------------------------------------------------------------
-- Explicit manipulation

bindVar :: ReportTypeErrors m => Var -> CtxBinding -> Ctx -> m Ctx
bindVar v binding ctx@MkCtx{ctxBindings, ctxDomain}
  | v `Set.member` ctxDomain =
      typeError ["Variable", ind [show v], "already bound in context."]
  | otherwise =
      pure
        ctx
          { ctxBindings = Right (v, binding) : ctxBindings
          , ctxDomain = Set.insert v ctxDomain
          }

-- | VarCtx
bindTermVar :: ReportTypeErrors m => Var -> Tipe -> Ctx -> m Ctx
bindTermVar v tipe ctx = do
  isWellFormedType ctx tipe
  bindVar v (HasType tipe) ctx

-- | UvarCtx
bindUniversal :: ReportTypeErrors m => Var -> Ctx -> m Ctx
bindUniversal v = bindVar v IsUniversal

-- | EvarCtx
bindOpenExistential :: ReportTypeErrors m => Var -> Ctx -> m Ctx
bindOpenExistential v = bindVar v (IsExistential Nothing)

-- | SolvedEvarCtx
bindSolvedExistential :: ReportTypeErrors m => Var -> Mono -> Ctx -> m Ctx
bindSolvedExistential v mono ctx = do
  isWellFormedMono ctx mono
  bindVar v (IsExistential (Just mono)) ctx

-- | MarkerCtx
markExistential :: ReportTypeErrors m => Var -> Ctx -> m Ctx
markExistential v ctx@MkCtx{ctxBindings, ctxDomain, ctxMarkers}
  | v `Set.member` ctxDomain =
      typeError ["Existential variable already exists in context", ind [show v]]
  | marker `Set.member` ctxMarkers =
      typeError ["Marker for existential variable already exists in context", ind [show v]]
  | otherwise =
      pure
        ctx
          { ctxBindings = Left marker : ctxBindings
          , ctxMarkers = Set.insert marker ctxMarkers
          }
 where
  marker = MkExistentialMarker v

extendCtx :: ReportTypeErrors m => Either ExistentialMarker (Var, CtxBinding) -> Ctx -> m Ctx
extendCtx = \case
  Left (MkExistentialMarker var) -> markExistential var
  Right (var, HasType tipe) -> bindTermVar var tipe
  Right (var, IsUniversal) -> bindUniversal var
  Right (var, IsExistential Nothing) -> bindOpenExistential var
  Right (var, IsExistential (Just solution)) -> bindSolvedExistential var solution

solveExistential :: ReportTypeErrors m => Var -> Mono -> Ctx -> m Ctx
solveExistential target = articulateExistential target []

articulateExistential :: ReportTypeErrors m => Var -> [Var] -> Mono -> Ctx -> m Ctx
articulateExistential target newVars solution ctx
  | target `Set.notMember` ctxDomain ctx =
      unknownVariableError
  | otherwise = do
      operateOnBindingHole_ target ctx \ctxPrefix binding -> do
        -- the target var must be a known unsolved existential
        case binding of
          Just (IsExistential Nothing) -> pure ()
          Just (IsExistential (Just alreadySolved)) ->
            typeError
              [ "Tried to articulate variable"
              , ind1 target
              , "to type"
              , ind1 solution
              , "but it was already instantiated to"
              , ind1 alreadySolved
              ]
          Just IsUniversal ->
            typeError ["Cannot articulate universal variable", ind1 target]
          Just (HasType _) ->
            typeError ["Cannot articulate term variable", ind1 target]
          Nothing -> unknownVariableError
        -- we then bind the new open variables, and bind the solution
        extendedCtxPrefix <- F.foldlM (flip bindOpenExistential) ctxPrefix newVars
        bindSolvedExistential target solution extendedCtxPrefix
 where
  unknownVariableError :: ReportTypeErrors m => m x
  unknownVariableError = typeError ["Cannot instantiate unknown variable", ind1 target]

operateOnBindingHole ::
  ReportTypeErrors m =>
  Var ->
  Ctx ->
  (Ctx -> Maybe CtxBinding -> m (b, Ctx)) ->
  m (b, Ctx)
operateOnBindingHole target ctx op = do
  -- we unroll the context until we find the target var
  let (ctxPrefix, binding, suffixEntries) = dropEntriesUntilBinding target ctx
  (result, ctxPrefix') <- op ctxPrefix binding
  -- and then replay all the entries we had unrolled
  restoredCtx <- F.foldlM (flip extendCtx) ctxPrefix' suffixEntries
  pure (result, restoredCtx)

operateOnBindingHole_ ::
  ReportTypeErrors m =>
  Var ->
  Ctx ->
  (Ctx -> Maybe CtxBinding -> m Ctx) ->
  m Ctx
operateOnBindingHole_ target ctx op = do
  snd <$> operateOnBindingHole target ctx \ctx' binding ->
    ((),) <$> op ctx' binding
