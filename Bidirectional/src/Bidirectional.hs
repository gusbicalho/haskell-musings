{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Bidirectional where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (State)
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

bindVar :: Var -> CtxBinding -> Ctx -> TC Ctx
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
bindTermVar :: Var -> Tipe -> Ctx -> TC Ctx
bindTermVar v tipe ctx = do
  isWellFormedType ctx tipe
  bindVar v (HasType tipe) ctx

-- | UvarCtx
bindUniversal :: Var -> Ctx -> TC Ctx
bindUniversal v = bindVar v IsUniversal

-- | EvarCtx
bindOpenExistential :: Var -> Ctx -> TC Ctx
bindOpenExistential v = bindVar v (IsExistential Nothing)

-- | SolvedEvarCtx
bindSolvedExistential :: Var -> Mono -> Ctx -> TC Ctx
bindSolvedExistential v mono ctx = do
  isWellFormedMono ctx mono
  bindVar v (IsExistential (Just mono)) ctx

-- | MarkerCtx
markExistential :: Var -> Ctx -> TC Ctx
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

extendCtx :: Either ExistentialMarker (Var, CtxBinding) -> Ctx -> TC Ctx
extendCtx = \case
  Left (MkExistentialMarker var) -> markExistential var
  Right (var, HasType tipe) -> bindTermVar var tipe
  Right (var, IsUniversal) -> bindUniversal var
  Right (var, IsExistential Nothing) -> bindOpenExistential var
  Right (var, IsExistential (Just solution)) -> bindSolvedExistential var solution

solveExistential :: Var -> Mono -> Ctx -> TC Ctx
solveExistential target = articulateExistential target []

articulateExistential :: Var -> [Var] -> Mono -> Ctx -> TC Ctx
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
  unknownVariableError :: TC x
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
  Var ->
  Ctx ->
  (Ctx -> Maybe CtxBinding -> TC (Ctx, b)) ->
  TC (Ctx, b)
operateOnBindingHole target ctx op = do
  -- we unroll the context until we find the target var
  let (ctxPrefix, binding, suffixEntries) = dropEntriesUntilBinding target ctx
  (ctxPrefix', result) <- op ctxPrefix binding
  -- and then replay all the entries we had unrolled
  restoredCtx <- F.foldlM (flip extendCtx) ctxPrefix' suffixEntries
  pure (restoredCtx, result)

operateOnBindingHole_ ::
  Var ->
  Ctx ->
  (Ctx -> Maybe CtxBinding -> ExceptT String (State Word) Ctx) ->
  ExceptT String (State Word) Ctx
operateOnBindingHole_ target ctx op = do
  fst <$> operateOnBindingHole target ctx \ctx' binding ->
    op ctx' binding <&> (,())

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

isWellFormedMono :: Ctx -> Mono -> TC ()
isWellFormedMono ctx = \case
  -- UnitWF
  MonoUnit -> pure ()
  -- UvarWF, EvarWF, SolvedEvarWF
  (MonoVar var) -> varIsBoundToAType ctx var
  -- ArrowWF
  (MonoFunction argType retType) -> do
    isWellFormedMono ctx argType
    isWellFormedMono ctx retType

isWellFormedType :: Ctx -> Tipe -> TC ()
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
    extendedCtx <- bindUniversal (NamedVar boundName) ctx
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
          instantiateToSubtypeOf ctx varA typeB
  -- InstantiateR
  go typeA (TVar varB)
    | Just (IsExistential Nothing) <- lookupBinding varB ctx =
        do
          varB `doesNotOccurFreeIn` typeA
          instantiateToSupertypeOf ctx typeA varB
  -- <:->
  go (TFunction argA retA) (TFunction argB retB) = do
    ctx' <- isSubtypeOf ctx argB argA
    let retA' = substCtxInType ctx' retA
    let retB' = substCtxInType ctx' retB
    isSubtypeOf ctx' retA' retB'
  -- <:ForallR
  go typeA (TForall univName univType) = do
    let univVar = NamedVar univName
    extendedCtx <- bindUniversal univVar ctx
    dirtyCtx <- isSubtypeOf extendedCtx typeA univType
    pure $ dropEntriesUntilBinding_ univVar dirtyCtx
  -- <=ForallL
  go (TForall boundName typeA) typeB = do
    existentialVar <- freshTypeVar "<=ForallL"
    existentialContext <-
      pure ctx
        >>= markExistential existentialVar
        >>= bindOpenExistential existentialVar
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
instantiateToSubtypeOf :: Ctx -> Var -> Tipe -> TC Ctx
instantiateToSubtypeOf ctx alpha = \case
  (TVar beta) -> case lookupBinding beta ctx of
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
      | definedBefore ctx alpha beta ->
          solveExistential beta (MonoVar alpha) ctx
    -- InstLSolve (via solved variable)
    Just (IsExistential (Just solved)) ->
      -- Really this should not happen, bacause we use substCtxInType before
      -- each recursion
      solveExistential alpha solved ctx
    -- InstLSolve (existential refers to outer universal or unsolved existential)
    Just _ ->
      solveExistential alpha (MonoVar beta) ctx
  -- InstLSolve (via unit-as-monotype)
  TUnit -> solveExistential alpha MonoUnit ctx
  -- InstLArr
  (TFunction argType retType) -> do
    argExists <- freshTypeVar "InstLArr_arg"
    retExists <- freshTypeVar "InstLArr_ret"
    articulatedCtx <-
      articulateExistential
        alpha
        [retExists, argExists]
        (MonoFunction (MonoVar argExists) (MonoVar retExists))
        ctx
    ctxAfterArgument <- instantiateToSupertypeOf articulatedCtx argType argExists
    instantiateToSubtypeOf ctxAfterArgument retExists (substCtxInType ctxAfterArgument retType)
  -- InstLAIIR
  (TForall univName univType) -> do
    let univVar = NamedVar univName
    extendedCtx <- bindUniversal univVar ctx
    dirtyCtx <- instantiateToSubtypeOf extendedCtx alpha univType
    pure $ dropEntriesUntilBinding_ univVar dirtyCtx

-- | Instantiate the var to a supertype of the type
instantiateToSupertypeOf :: Ctx -> Tipe -> Var -> TC Ctx
instantiateToSupertypeOf ctx = flip go
 where
  go alpha = \case
    (TVar beta) -> case lookupBinding beta ctx of
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
        | definedBefore ctx alpha beta ->
            solveExistential beta (MonoVar alpha) ctx
      -- InstRSolve (via solved variable)
      Just (IsExistential (Just solved)) ->
        -- Really this should not happen, bacause we use substCtxInType before
        -- each recursion
        solveExistential alpha solved ctx
      -- InstRSolve (existential refers to outer universal or unsolved existential)
      Just _ ->
        solveExistential alpha (MonoVar beta) ctx
    -- InstRSolve (via unit-as-monotype)
    TUnit -> solveExistential alpha MonoUnit ctx
    -- InstRArr
    TFunction argType retType -> do
      argExists <- freshTypeVar "InstRArr_arg"
      retExists <- freshTypeVar "InstRArr_ret"
      articulatedCtx <-
        articulateExistential
          alpha
          [retExists, argExists]
          (MonoFunction (MonoVar argExists) (MonoVar retExists))
          ctx
      ctxAfterArgument <- instantiateToSubtypeOf articulatedCtx argExists argType
      instantiateToSupertypeOf ctxAfterArgument (substCtxInType ctxAfterArgument retType) retExists
    -- InstRAIIL
    (TForall univName univType) -> do
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
      extendedCtx <-
        pure ctx
          >>= markExistential univVar
          >>= bindOpenExistential univVar
      dirtyCtx <- instantiateToSupertypeOf extendedCtx univType alpha
      pure $ dropEntriesUntilMarkerOf univVar dirtyCtx

varIsBoundToAType :: Ctx -> Var -> TC ()
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

varIsBoundToATerm :: Ctx -> Var -> TC Tipe
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
  (finalCtx, tipe) <- typeSynth emptyCtx expr
  pure $ substCtxInType finalCtx tipe

typeSynth :: Ctx -> Expr -> TC (Ctx, Tipe)
typeSynth ctx = goSynth
 where
  goSynth :: Expr -> TC (Ctx, Tipe)
  goSynth = \case
    -- Var
    EVar varName -> (ctx,) <$> varIsBoundToATerm ctx varName
    -- Anno
    EAnno expr tipe -> do
      isWellFormedType ctx tipe
      ctx' <- typeCheck ctx expr tipe
      pure (ctx', tipe)
    -- ->E
    expr@(EApply callee argument) -> do
      (ctxAfterCallee, calleeType) <- goSynth callee
      typeApply expr ctxAfterCallee (substCtxInType ctxAfterCallee calleeType) argument
    -- 1I⇒
    EUnit -> pure (ctx, TUnit)
    -- ->I⇒
    ELam argName body -> do
      let argVar = NamedVar argName
      alpha <- freshTypeVar "->I⇒_arg"
      let argType = TVar alpha
      beta <- freshTypeVar "->I⇒_ret"
      let retType = TVar beta
      extendedCtx <-
        pure ctx
          >>= bindOpenExistential alpha
          >>= bindOpenExistential beta
          >>= bindTermVar argVar argType
      dirtyCtx <- typeCheck extendedCtx body retType
      pure
        ( dropEntriesUntilBinding_ argVar dirtyCtx
        , TFunction argType retType
        )

typeCheck :: Ctx -> Expr -> Tipe -> TC Ctx
typeCheck ctx = goCheck
 where
  -- ForallI
  goCheck expr (TForall univName univType) = do
    let forallVar = NamedVar univName
    extendedCtx <- bindUniversal forallVar ctx
    dirtyCtx <- typeCheck extendedCtx expr univType
    pure (dropEntriesUntilBinding_ forallVar dirtyCtx)
  -- ->I
  goCheck (ELam argName body) (TFunction argType retType) = do
    let argVar = NamedVar argName
    extendedCtx <- bindTermVar argVar argType ctx
    dirtyCtx <- typeCheck extendedCtx body retType
    pure (dropEntriesUntilBinding_ argVar dirtyCtx)
  -- 1I
  goCheck EUnit TUnit = pure ctx
  -- Sub
  goCheck expr expectedType = do
    (ctxAfterSynth, foundType) <- typeSynth ctx expr
    isSubtypeOf
      ctxAfterSynth
      (substCtxInType ctxAfterSynth foundType)
      (substCtxInType ctxAfterSynth expectedType)
      `Except.catchE` \err ->
        typeError
          [ err
          , "when typechecking expression"
          , ind [show expr]
          ]

typeApply :: Expr -> Ctx -> Tipe -> Expr -> TC (Ctx, Tipe)
typeApply overallApplyExpr = goApply
 where
  -- ForallApp
  goApply ctx (TForall univName univType) argument = do
    extendedCtx <- bindOpenExistential (NamedVar univName) ctx
    goApply extendedCtx univType argument
  -- ->App
  goApply ctx (TFunction argType retType) argument = do
    typeCheck ctx argument argType <&> (,retType)
  -- ExistApp
  goApply ctx (TVar alpha) argument
    | Just (IsExistential Nothing) <- lookupBinding alpha ctx = do
        argVar <- freshTypeVar "ExistApp_arg"
        retVar <- freshTypeVar "ExistApp_ret"
        articulatedCtx <-
          articulateExistential
            alpha
            [retVar, argVar]
            (MonoFunction (MonoVar argVar) (MonoVar retVar))
            ctx
        typeCheck articulatedCtx argument (TVar argVar) <&> (,TVar retVar)
  goApply _ctx other _ =
    typeError $
      [ "Expected polymorphic or function type, but got"
      , ind [show other]
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
type TC a = ExceptT String (State Word) a

runTC :: TC a -> Either String a
runTC = flip State.evalState 0 . Except.runExceptT

-- fresh type var

freshTypeVar :: String -> TC Var
freshTypeVar tag = do
  fresh <- Trans.lift State.get
  Trans.lift $ State.modify' succ
  pure $ FreshVar tag fresh

-- Error reporting

typeError :: [String] -> TC a
typeError = Except.throwE . List.intercalate "\n"

ind :: [String] -> String
ind = List.intercalate "\n" . fmap ("  " <>)

ind1 :: Show a => a -> String
ind1 x = ind [show x]

expectRight :: TC a -> a
expectRight = either error id . runTC

orFailWith :: Maybe a -> [String] -> TC a
mb `orFailWith` err =
  case mb of
    Just a -> pure a
    Nothing -> typeError err

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
