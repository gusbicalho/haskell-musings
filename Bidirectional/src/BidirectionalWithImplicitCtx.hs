{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module BidirectionalWithImplicitCtx where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set

data Var
  = NamedVar String
  | FreshVar String Word
  deriving stock (Eq, Ord, Show)

data Expr
  = EUnit
  | EVar Var
  | EAnno Expr Tipe
  | ELam String Expr
  | EApply Expr Expr
  deriving stock (Eq, Ord, Show)

data Mono
  = MonoUnit
  | MonoVar Var
  | MonoFunction Mono Mono
  deriving stock (Eq, Ord, Show)

data Tipe
  = TUnit
  | TVar Var
  | TFunction Tipe Tipe
  | TForall String Tipe
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Mono manipulation

monoToTipe :: Mono -> Tipe
monoToTipe = \case
  MonoUnit -> TUnit
  MonoVar v -> TVar v
  MonoFunction arg ret -> TFunction (monoToTipe arg) (monoToTipe ret)

tipeToMono :: Tipe -> Maybe Mono
tipeToMono = \case
  TUnit -> Just MonoUnit
  TVar v -> Just (MonoVar v)
  TFunction arg ret -> MonoFunction <$> tipeToMono arg <*> tipeToMono ret
  TForall{} -> Nothing

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

bindVar :: ReportTypeErrors m => Var -> CtxBinding -> CtxStateT m ()
bindVar v binding = getCtx >>= go
 where
  go ctx@MkCtx{ctxBindings, ctxDomain}
    | v `Set.member` ctxDomain =
        typeError ["Variable", ind [show v], "already bound in context."]
    | otherwise =
        putCtx
          ctx
            { ctxBindings = Right (v, binding) : ctxBindings
            , ctxDomain = Set.insert v ctxDomain
            }

-- | VarCtx
bindTermVar :: ReportTypeErrors m => Var -> Tipe -> CtxStateT m ()
bindTermVar v tipe = do
  getCtx >>= \ctx -> isWellFormedType ctx tipe
  bindVar v (HasType tipe)

-- | UvarCtx
bindUniversal :: ReportTypeErrors m => Var -> CtxStateT m ()
bindUniversal v = bindVar v IsUniversal

-- | EvarCtx
bindOpenExistential :: ReportTypeErrors m => Var -> CtxStateT m ()
bindOpenExistential v = bindVar v (IsExistential Nothing)

-- | SolvedEvarCtx
bindSolvedExistential :: ReportTypeErrors m => Var -> Mono -> CtxStateT m ()
bindSolvedExistential v mono = do
  getCtx >>= \ctx -> isWellFormedMono ctx mono
  bindVar v (IsExistential (Just mono))

-- | MarkerCtx
markExistential :: ReportTypeErrors m => Var -> CtxStateT m ()
markExistential v = getCtx >>= go
 where
  go ctx@MkCtx{ctxBindings, ctxDomain, ctxMarkers}
    | v `Set.member` ctxDomain =
        typeError ["Existential variable already exists in context", ind [show v]]
    | marker `Set.member` ctxMarkers =
        typeError ["Marker for existential variable already exists in context", ind [show v]]
    | otherwise =
        putCtx
          ctx
            { ctxBindings = Left marker : ctxBindings
            , ctxMarkers = Set.insert marker ctxMarkers
            }
  marker = MkExistentialMarker v

extendCtx :: ReportTypeErrors m => Either ExistentialMarker (Var, CtxBinding) -> CtxStateT m ()
extendCtx = \case
  Left (MkExistentialMarker var) -> markExistential var
  Right (var, HasType tipe) -> bindTermVar var tipe
  Right (var, IsUniversal) -> bindUniversal var
  Right (var, IsExistential Nothing) -> bindOpenExistential var
  Right (var, IsExistential (Just solution)) -> bindSolvedExistential var solution

solveExistential :: Var -> Mono -> Ctx -> TC Ctx
solveExistential target mono ctx = execCtx ctx $ articulateExistential target [] mono

articulateExistential :: ReportTypeErrors m => Var -> [Var] -> Mono -> CtxStateT m ()
articulateExistential target newVars solution = do
  ctx <- getCtx
  if target `Set.notMember` ctxDomain ctx
    then unknownVariableError
    else do
      operateOnBindingHole_ target \binding -> do
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
        F.traverse_ bindOpenExistential newVars
        bindSolvedExistential target solution
 where
  unknownVariableError :: ReportTypeErrors m => m x
  unknownVariableError = typeError ["Cannot instantiate unknown variable", ind1 target]

lookupBinding :: Var -> Ctx -> Maybe CtxBinding
lookupBinding var MkCtx{ctxBindings, ctxDomain}
  | var `Set.notMember` ctxDomain = Nothing
  | otherwise = Maybe.listToMaybe . Maybe.mapMaybe check $ ctxBindings
 where
  check (Left _) = Nothing
  check (Right (v, binding))
    | var == v = Just binding
    | otherwise = Nothing

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

substCtxInType :: Ctx -> Tipe -> Tipe
substCtxInType ctx = go
 where
  go TUnit = TUnit
  go t@(TVar var) = case lookupBinding var ctx of
    Just (IsExistential (Just solved)) -> go (monoToTipe solved)
    _ -> t
  go (TFunction argType retType) = TFunction (go argType) (go retType)
  go (TForall universalName tipe) = TForall universalName (go tipe)

dropEntriesUntilBinding :: Var -> Ctx -> (Ctx, Maybe CtxBinding, [Either ExistentialMarker (Var, CtxBinding)])
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

operateOnBindingHole ::
  ReportTypeErrors m =>
  Var ->
  (Maybe CtxBinding -> CtxStateT m b) ->
  CtxStateT m b
operateOnBindingHole target op = do
  -- we unroll the context until we find the target var
  ctx <- getCtx
  let (ctxPrefix, binding, suffixEntries) = dropEntriesUntilBinding target ctx
  putCtx ctxPrefix
  result <- op binding
  -- and then replay all the entries we had unrolled
  F.traverse_ extendCtx suffixEntries
  pure result

operateOnBindingHole_ ::
  ReportTypeErrors m =>
  Var ->
  (Maybe CtxBinding -> CtxStateT m ()) ->
  CtxStateT m ()
operateOnBindingHole_ target op = do
  fst <$> operateOnBindingHole target \binding ->
    op binding <&> (,())

dropEntriesUntilBinding_ :: Var -> Ctx -> Ctx
dropEntriesUntilBinding_ target = (\(ctx, _, _) -> ctx) . dropEntriesUntilBinding target

dropEntriesUntilMarkerOf :: Var -> Ctx -> Ctx
dropEntriesUntilMarkerOf target = go
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
-- Well-formedness of types and subtyping
--
-- Here I decided to manage Ctx explicitly in the top-level defs, because there
-- is a lot of checking the initial state in pattern matches, and also places
-- where we need to _speculatively_ extend the Ctx, but we don't want to
-- propagate changes (e.g. ForallWF).
-- I used in a few places `execCtx` to bridge the world with the implicit Ctx
-- management machinery defined above.

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
  TForall boundName boundType -> do
    extendedCtx <- execCtx ctx do
      bindUniversal (NamedVar boundName)
    isWellFormedType extendedCtx boundType

doesNotOccurFreeIn :: Var -> Tipe -> TC ()
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
  go (TForall univName univType)
    -- shadowing
    | NamedVar univName == target = pure ()
    -- no shadowing
    | otherwise = go univType
  go TUnit = pure ()
  go (TFunction argType retType) = go argType *> go retType

isSubtypeOf :: Ctx -> Tipe -> Tipe -> TC Ctx
isSubtypeOf ctx = go
 where
  -- <:Unit
  go TUnit TUnit = pure ctx
  -- <:Var, <:Exvar
  go (TVar v1) (TVar v2)
    | v1 == v2 = do
        varIsBoundToAType ctx v1
        pure ctx
  -- InstantiateL
  go (TVar varA) typeB
    | Just (IsExistential Nothing) <- lookupBinding varA ctx =
        do
          varA `doesNotOccurFreeIn` typeB
          instantiateToSubtypeOf varA typeB ctx
  -- InstantiateR
  go typeA (TVar varB)
    | Just (IsExistential Nothing) <- lookupBinding varB ctx =
        do
          varB `doesNotOccurFreeIn` typeA
          instantiateToSupertypeOf typeA varB ctx
  -- <:->
  go (TFunction argA retA) (TFunction argB retB) = do
    ctx' <- isSubtypeOf ctx argB argA
    let retA' = substCtxInType ctx' retA
    let retB' = substCtxInType ctx' retB
    isSubtypeOf ctx' retA' retB'
  -- <:ForallR
  go typeA (TForall univName univType) = do
    let univVar = NamedVar univName
    extendedCtx <- execCtx ctx $ bindUniversal univVar
    dirtyCtx <- isSubtypeOf extendedCtx typeA univType
    pure $ dropEntriesUntilBinding_ univVar dirtyCtx
  -- <=ForallL
  go (TForall boundName typeA) typeB = do
    existentialVar <- freshTypeVar "<=ForallL"
    existentialContext <- execCtx ctx do
      markExistential existentialVar
      bindOpenExistential existentialVar
    let boundA = substType (NamedVar boundName) (TVar existentialVar) typeA
    dirtyCtx <- isSubtypeOf existentialContext boundA typeB
    pure $ dropEntriesUntilMarkerOf existentialVar dirtyCtx
  -- otherwise, fail!
  go a b =
    typeError
      [ "Type error."
      , ind [show a]
      , "is not a subtype of"
      , ind [show b]
      ]

-- | Instantiate the var to a subtype of the type
instantiateToSubtypeOf :: Var -> Tipe -> Ctx -> TC Ctx
instantiateToSubtypeOf = \v t ctx -> go ctx v t
 where
  go :: Ctx -> Var -> Tipe -> TC Ctx
  go baseCtx alpha = \case
    (TVar beta) -> do
      case lookupBinding beta baseCtx of
        Nothing ->
          typeError ["Unbound variable (2)", ind1 beta]
        Just (HasType termType) ->
          typeError
            [ "Cannot instantiate existential to term variable."
            , ind1 beta
            , "has type"
            , ind1 termType
            ]
        -- InstLReach
        Just (IsExistential Nothing)
          | definedBefore baseCtx alpha beta ->
              solveExistential beta (MonoVar alpha) baseCtx
        -- InstLSolve (via solved variable)
        Just (IsExistential (Just solved)) ->
          -- Really this should not happen, bacause we use substCtxInType before
          -- each recursion
          solveExistential alpha solved baseCtx
        -- InstLSolve (existential refers to outer universal or unsolved existential)
        Just _ ->
          solveExistential alpha (MonoVar beta) baseCtx
    -- InstLSolve (via unit-as-monotype)
    TUnit -> solveExistential alpha MonoUnit baseCtx
    -- InstLArr
    (TFunction argType retType) -> do
      argExists <- freshTypeVar "InstLArr_arg"
      retExists <- freshTypeVar "InstLArr_ret"
      execCtx baseCtx do
        articulateExistential
          alpha
          [retExists, argExists]
          (MonoFunction (MonoVar argExists) (MonoVar retExists))
        onCtx_ $ instantiateToSupertypeOf argType argExists
        retType' <- getCtx <&> \ctx -> substCtxInType ctx retType
        onCtx_ $ instantiateToSubtypeOf retExists retType'
    -- InstLAIIR
    (TForall univName univType) -> execCtx baseCtx do
      let univVar = NamedVar univName
      bindUniversal univVar
      onCtx_ $ instantiateToSubtypeOf alpha univType
      onCtx_ $ pure . dropEntriesUntilBinding_ univVar

-- | Instantiate the var to a supertype of the type
instantiateToSupertypeOf :: Tipe -> Var -> Ctx -> TC Ctx
instantiateToSupertypeOf = \t v ctx -> go ctx v t
 where
  go :: Ctx -> Var -> Tipe -> TC Ctx
  go baseCtx alpha = \case
    (TVar beta) -> case lookupBinding beta baseCtx of
      Nothing ->
        typeError ["Unbound variable (1)", ind1 beta]
      Just (HasType termType) ->
        typeError
          [ "Cannot instantiate existential to term variable."
          , ind1 beta
          , "has type"
          , ind1 termType
          ]
      -- InstRReach
      Just (IsExistential Nothing)
        | definedBefore baseCtx alpha beta ->
            solveExistential beta (MonoVar alpha) baseCtx
      -- InstRSolve (via solved variable)
      Just (IsExistential (Just solved)) ->
        -- Really this should not happen, bacause we use substCtxInType before
        -- each recursion
        solveExistential alpha solved baseCtx
      -- InstRSolve (existential refers to outer universal or unsolved existential)
      Just _ ->
        solveExistential alpha (MonoVar beta) baseCtx
    -- InstRSolve (via unit-as-monotype)
    TUnit -> solveExistential alpha MonoUnit baseCtx
    -- InstRArr
    TFunction argType retType -> execCtx baseCtx do
      argExists <- freshTypeVar "InstRArr_arg"
      retExists <- freshTypeVar "InstRArr_ret"
      articulateExistential
        alpha
        [retExists, argExists]
        (MonoFunction (MonoVar argExists) (MonoVar retExists))
      onCtx_ $ instantiateToSubtypeOf argExists argType
      retType' <- getCtx <&> \ctx -> substCtxInType ctx retType
      onCtx_ $ instantiateToSupertypeOf retType' retExists
    -- InstRAIIL
    (TForall univName univType) -> execCtx baseCtx do
      let univVar = NamedVar univName
      -- If alpha should be a supertype of a Forall, it cannot be a monotype,
      -- so indeed we should not solve the existential here!
      -- A new existential variable is created so that typechecking can proceeed
      -- inside the Forall type, but whatever is learned about this variable
      -- never escapes.
      -- This excerpt from page 8 helps:
      -- "Here, we introduce a new variable α^ to go under the universal
      --  quantifier; then, instantiation applies InstRReach to set α^, not β^.
      --  Hence, β^ is, correctly, not constrained by this subtyping problem"
      markExistential univVar
      bindOpenExistential univVar
      onCtx_ \ctx -> instantiateToSupertypeOf univType alpha ctx
      onCtx_ $ pure . dropEntriesUntilMarkerOf univVar

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

--------------------------------------------------------------------------------
-- Type checking

typeComplete :: Expr -> TC Tipe
typeComplete expr = do
  (tipe, finalCtx) <- runCtx emptyCtx (typeSynth expr)
  pure $ substCtxInType finalCtx tipe

typeSynth :: Expr -> CtxStateT TC Tipe
typeSynth = goSynth
 where
  goSynth :: Expr -> CtxStateT TC Tipe
  goSynth = \case
    -- Var
    EVar varName ->
      withCtx \ctx -> varIsBoundToATerm ctx varName
    -- Anno
    EAnno expr tipe -> do
      withCtx \ctx -> isWellFormedType ctx tipe
      typeCheck expr tipe
      pure tipe
    -- ->E
    expr@(EApply callee argument) -> do
      calleeType <- do
        calleeType <- goSynth callee
        ctx <- getCtx
        pure $ substCtxInType ctx calleeType
      typeApply expr calleeType argument
    -- 1I⇒
    EUnit -> pure TUnit
    -- ->I⇒
    ELam argName body -> do
      let argVar = NamedVar argName
      alpha <- freshTypeVar "->I⇒_arg"
      let argType = TVar alpha
      beta <- freshTypeVar "->I⇒_ret"
      let retType = TVar beta
      bindOpenExistential alpha
      bindOpenExistential beta
      bindTermVar argVar argType
      typeCheck body retType
      onCtx_ $ pure . dropEntriesUntilBinding_ argVar
      pure (TFunction argType retType)

typeCheck :: Expr -> Tipe -> CtxStateT TC ()
typeCheck = goCheck
 where
  goCheck :: Expr -> Tipe -> CtxStateT TC ()
  -- ForallI
  goCheck expr (TForall univName univType) = do
    let forallVar = NamedVar univName
    bindUniversal forallVar
    goCheck expr univType
    onCtx_ $ pure . dropEntriesUntilBinding_ forallVar
  -- ->I
  goCheck (ELam argName body) (TFunction argType retType) = do
    let argVar = NamedVar argName
    bindTermVar argVar argType
    goCheck body retType
    onCtx_ $ pure . dropEntriesUntilBinding_ argVar
  -- 1I
  goCheck EUnit TUnit = pure ()
  -- Sub
  goCheck expr expectedType = do
    foundType <- typeSynth expr
    onCtx_ \ctx ->
      isSubtypeOf
        ctx
        (substCtxInType ctx foundType)
        (substCtxInType ctx expectedType)
        `catchTypeError` \err ->
          typeError
            [ err
            , "when typechecking expression"
            , ind [show expr]
            ]

typeApply :: Expr -> Tipe -> Expr -> CtxStateT TC Tipe
typeApply overallApplyExpr = goApply
 where
  goApply :: Tipe -> Expr -> CtxStateT TC Tipe
  -- ForallApp
  goApply (TForall univName univType) argument = do
    bindOpenExistential (NamedVar univName)
    goApply univType argument
  -- ->App
  goApply (TFunction argType retType) argument = do
    typeCheck argument argType
    pure retType
  -- ExistApp
  goApply tipe@(TVar alpha) argument = do
    lookupBinding alpha <$> getCtx >>= \case
      Just (IsExistential Nothing) -> do
        argVar <- freshTypeVar "ExistApp_arg"
        retVar <- freshTypeVar "ExistApp_ret"
        articulateExistential
          alpha
          [retVar, argVar]
          (MonoFunction (MonoVar argVar) (MonoVar retVar))
        typeCheck argument (TVar argVar)
        pure (TVar retVar)
      _ -> badApplyError tipe
  goApply tipe _ = badApplyError tipe
  badApplyError tipe =
    typeError $
      [ "Expected polymorphic or function type, but got"
      , ind [show tipe]
      , "in application expression"
      , ind [show overallApplyExpr]
      ]

substType :: Var -> Tipe -> Tipe -> Tipe
substType replacedVar replacement = go
 where
  go TUnit = TUnit
  go (TFunction argType retType) = TFunction (go argType) (go retType)
  go t@(TVar var)
    | var == replacedVar = replacement
    | otherwise = t
  go t@(TForall boundVarName boundType)
    -- shadowing
    | NamedVar boundVarName == replacedVar = t
    -- no shadowing
    | otherwise = TForall boundVarName (go boundType)

-- Effects stack
type TC = StateT Word (Except String)
type CtxStateT m = StateT Ctx m

runCtx :: Ctx -> CtxStateT m a -> m (a, Ctx)
runCtx = flip State.runStateT

execCtx :: Monad m => Ctx -> CtxStateT m () -> m Ctx
execCtx = flip State.execStateT

evalCtx :: Monad m => Ctx -> CtxStateT m a -> m a
evalCtx = flip State.evalStateT

runTC :: TC a -> Either String a
runTC = Except.runExcept . flip State.evalStateT 0

-- Implicit context management

getCtx :: Monad m => CtxStateT m Ctx
getCtx = State.get

putCtx :: Monad m => Ctx -> CtxStateT m ()
putCtx = State.put

onCtx :: Monad m => (Ctx -> m (a, Ctx)) -> CtxStateT m a
onCtx = State.StateT

onCtx_ :: Monad m => (Ctx -> m Ctx) -> CtxStateT m ()
onCtx_ f = onCtx \ctx -> ((),) <$> f ctx

withCtx :: Monad m => (Ctx -> m a) -> CtxStateT m a
withCtx f = Trans.lift . f =<< getCtx

-- fresh type var

class FreshTypeVar tc where
  freshTypeVar :: String -> tc Var

instance FreshTypeVar TC where
  freshTypeVar tag = do
    fresh <- State.get
    State.modify' succ
    pure $ FreshVar tag fresh

instance MonadTrans mt => FreshTypeVar (mt TC) where
  freshTypeVar = Trans.lift . freshTypeVar

-- Error reporting

class Monad tc => ReportTypeErrors tc where
  reportTypeError :: String -> tc a
  catchTypeError :: tc a -> (String -> tc a) -> tc a

instance ReportTypeErrors TC where
  reportTypeError = Trans.lift . Except.throwE
  catchTypeError = State.liftCatch Except.catchE

instance (Monad m, ReportTypeErrors m) => ReportTypeErrors (CtxStateT m) where
  reportTypeError = Trans.lift . reportTypeError
  catchTypeError = State.liftCatch catchTypeError

typeError :: ReportTypeErrors tc => [String] -> tc a
typeError = reportTypeError . List.intercalate "\n"

ind :: [String] -> String
ind = List.intercalate "\n" . fmap ("  " <>)

ind1 :: Show a => a -> String
ind1 x = ind [show x]

expectRight :: TC a -> a
expectRight = either error id . runTC

--------------------------------------------------------------------------------
-- Example

test_id :: Expr
test_id = ELam "x" (EVar (NamedVar "x"))

test_const :: Expr
test_const = ELam "x" (ELam "y" (EVar (NamedVar "x")))

test_constUnit :: Expr
test_constUnit = EApply test_const EUnit

{-

>>> expectRight . typeComplete $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> expectRight . typeComplete $ EAnno test_id (TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T"))))
TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> expectRight . typeComplete $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> expectRight . typeComplete $ ELam "x" EUnit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> expectRight . typeComplete $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

-}
