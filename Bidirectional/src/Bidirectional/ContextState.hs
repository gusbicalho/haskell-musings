{-# LANGUAGE BlockArguments #-}

module Bidirectional.ContextState (
  CtxStateT,
  runCtx,
  execCtx,
  evalCtx,
  getCtx,
  onCtx,
  withCtx,
  -- Manipulation
  articulateExistential,
  markExistential,
  bindOpenExistential,
  bindUniversal,
  bindTermVar,
  dropEntriesUntilBinding,
  dropEntriesUntilBinding_,
  dropEntriesUntilMarkerOf,
  defaultAllUnsolvedExistentials,
  -- Queries
  varIsBoundToATerm,
  varIsBoundToAType,
) where

import Bidirectional.Context (Ctx)
import Bidirectional.Context qualified as Ctx
import Bidirectional.Language (Tipe, Var)
import Bidirectional.ReportTypeErrors (ReportTypeErrors)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT

--------------------------------------------------------------------------------
-- Implicit manipulation of Ctx in a State monad

-- | VarCtx
bindTermVar :: ReportTypeErrors m => Var -> Tipe -> CtxStateT m ()
bindTermVar v tipe = onCtx $ Ctx.bindTermVar v tipe

-- | UvarCtx
bindUniversal :: ReportTypeErrors m => Var -> CtxStateT m ()
bindUniversal v = onCtx $ Ctx.bindUniversal v

-- | EvarCtx
bindOpenExistential :: ReportTypeErrors m => Var -> CtxStateT m ()
bindOpenExistential v = onCtx $ Ctx.bindOpenExistential v

-- | MarkerCtx
markExistential :: ReportTypeErrors m => Var -> CtxStateT m ()
markExistential v = onCtx $ Ctx.markExistential v

articulateExistential :: ReportTypeErrors m => Var -> [Var] -> Ctx.Mono -> CtxStateT m ()
articulateExistential target newVars solution =
  onCtx $ Ctx.articulateExistential target newVars solution

dropEntriesUntilBinding ::
  Monad m =>
  Var ->
  CtxStateT
    m
    ( Maybe Ctx.CtxBinding
    , [Either Ctx.ExistentialMarker (Var, Ctx.CtxBinding)]
    )
dropEntriesUntilBinding var = do
  ctx <- getCtx
  let (ctx', binding, droppedEntries) = Ctx.dropEntriesUntilBinding var ctx
  putCtx ctx'
  pure (binding, droppedEntries)

dropEntriesUntilBinding_ :: Monad m => Var -> CtxStateT m ()
dropEntriesUntilBinding_ var = onCtx $ pure . Ctx.dropEntriesUntilBinding_ var

dropEntriesUntilMarkerOf ::
  Monad m =>
  Var ->
  CtxStateT
    m
    ( Maybe Ctx.ExistentialMarker
    , [Either Ctx.ExistentialMarker (Var, Ctx.CtxBinding)]
    )
dropEntriesUntilMarkerOf var = do
  ctx <- getCtx
  let (ctx', marker, droppedEntries) = Ctx.dropEntriesUntilMarkerOf var ctx
  putCtx ctx'
  pure (marker, droppedEntries)

defaultAllUnsolvedExistentials :: ReportTypeErrors m => Ctx.Mono -> CtxStateT m ()
defaultAllUnsolvedExistentials = onCtx . Ctx.defaultAllUnsolvedExistentials

--------------------------------------------------------------------------------
-- Queries to the implicit state

varIsBoundToATerm :: ReportTypeErrors m => Var -> CtxStateT m Tipe
varIsBoundToATerm varName = withCtx $ flip Ctx.varIsBoundToATerm varName

varIsBoundToAType :: ReportTypeErrors m => Var -> CtxStateT m ()
varIsBoundToAType varName = withCtx $ flip Ctx.varIsBoundToAType varName

--------------------------------------------------------------------------------
-- Monad transformer

type CtxStateT m = StateT Ctx m

runCtx :: Ctx -> CtxStateT m a -> m (a, Ctx)
runCtx = flip StateT.runStateT

execCtx :: Monad m => Ctx -> CtxStateT m () -> m Ctx
execCtx = flip StateT.execStateT

evalCtx :: Monad m => Ctx -> CtxStateT m a -> m a
evalCtx = flip StateT.evalStateT

getCtx :: Monad m => CtxStateT m Ctx
getCtx = StateT.get

putCtx :: Monad m => Ctx -> CtxStateT m ()
putCtx = StateT.put

onCtx :: Monad m => (Ctx -> m Ctx) -> CtxStateT m ()
onCtx f = StateT.StateT \ctx -> ((),) <$> f ctx

withCtx :: Monad m => (Ctx -> m a) -> CtxStateT m a
withCtx f = Trans.lift . f =<< getCtx
