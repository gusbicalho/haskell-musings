{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LambdaPi.Dependent.MyExtensions.Vec where

import LambdaPi.Dependent.MyExtensions.Core
import LambdaPi.Dependent.MyExtensions.Nat

data VecExt

data TermVec ext
  = Vec (TermChk ext) (TermChk ext)
  | Nil (TermChk ext)
  | Cons (TermChk ext) (TermChk ext) (TermChk ext) (TermChk ext)
  | VecElim (TermChk ext) (TermChk ext) (TermChk ext) (TermChk ext) (TermChk ext) (TermChk ext)

deriving stock instance Eq (TermChk ext) => Eq (TermVec ext)
deriving stock instance Show (TermChk ext) => Show (TermVec ext)

data ValueVec ext
  = VVecT (Value ext) (Value ext)
  | VNil (Value ext)
  | VCons (Value ext) (Value ext) (Value ext) (Value ext)

data NeutralVec ext
  = NVecElim (Value ext) (Value ext) (Value ext) (Value ext) (Value ext) (Neutral ext)

instance
  ( Extension extSet
  , TypeExtension NatExt extSet
  , Includes (ExtTerm extSet extSet) (TermVec extSet)
  , Includes (ExtNeutral extSet extSet) (NeutralVec extSet)
  , Includes (ExtValue extSet extSet) (ValueVec extSet)
  ) =>
  TypeExtension VecExt extSet
  where
  type ExtTerm VecExt extSet = TermVec extSet
  type ExtValue VecExt extSet = ValueVec extSet
  type ExtNeutral VecExt extSet = NeutralVec extSet
  typeExt = typeVec

instance TypeExtension VecExt ext => Eval (TermVec ext) ext where
  eval = evalVec

instance TypeExtension VecExt ext => Subst (TermVec ext) ext where
  subst = substVec

instance Extension extSet => Quote (ValueVec extSet) (TermVec extSet) where
  quote = quoteVec

instance Extension extSet => Quote (NeutralVec extSet) (TermVec extSet) where
  quote = quoteNeutralVec

-- Impls

evalVec ::
  forall extSet.
  TypeExtension VecExt extSet =>
  (TermVec extSet) ->
  Env extSet ->
  (Value extSet)
evalVec term env =
  case term of
    Vec e size -> vVecT (eval e env) (eval size env)
    Nil e -> vNil (eval e env)
    Cons e tailSize v tail ->
      vCons
        (eval e env)
        (eval tailSize env)
        (eval v env)
        (eval tail env)
    VecElim e mot base step size xs ->
      let e' = eval e env
          mot' = eval mot env
          base' = eval base env
          step' = eval step env
          go n = \case
            VNeutral k -> VNeutral . NExt . inject $ NVecElim e' mot' base' step' n k
            VExt (projVec -> Just (VNil _)) -> base'
            VExt (projVec -> Just (VCons _ tailSize x tail)) ->
              step' `vapp` tailSize `vapp` x `vapp` tail `vapp` go tailSize tail
            other -> error $ "vecElim on non-Vec: " <> show (quote0 other)
       in go (eval size env) (eval xs env)
 where
  projVec = project @(ExtValue extSet extSet) @(ValueVec extSet)
  injVec = VExt . inject @(ExtValue extSet extSet) @(ValueVec extSet)
  vVecT e size = injVec $ VVecT e size
  vNil = injVec . VNil
  vCons e l x xs = injVec (VCons e l x xs)

quoteVec :: Extension ext => Word -> (ValueVec ext) -> (TermVec ext)
quoteVec n = \case
  VVecT e size -> Vec (quote n e) (quote n size)
  VNil e -> Nil (quote n e)
  VCons e tailSize x tail -> Cons (quote n e) (quote n tailSize) (quote n x) (quote n tail)

quoteNeutralVec :: Extension ext => Word -> (NeutralVec ext) -> (TermVec ext)
quoteNeutralVec n = \case
  NVecElim e mot base step size xs ->
    VecElim
      (quote n e)
      (quote n mot)
      (quote n base)
      (quote n step)
      (quote n size)
      (quote n (VNeutral xs))

typeVec ::
  forall ext.
  ( TypeExtension NatExt ext
  , TypeExtension VecExt ext
  ) =>
  Word ->
  (Context ext) ->
  (TermVec ext) ->
  Result (Value ext)
typeVec i ctx = \case
  Vec e size -> do
    checkType i ctx e VStar
    checkType i ctx size vNatT
    pure VStar
  Nil e -> do
    checkType i ctx e VStar
    pure (vVecT (eval0 e) vZero)
  Cons e tailSize x tail -> do
    checkType i ctx e VStar
    let e' = eval0 e
    checkType i ctx tailSize vNatT
    let tailSize' = eval0 tailSize
    checkType i ctx x e'
    checkType i ctx tail (vVecT e' tailSize')
    pure (vVecT e' (vSucc tailSize'))
  VecElim e mot base step size xs -> do
    checkType i ctx e VStar
    let e' = eval0 e
    checkType i ctx mot (VPi vNatT \size -> VPi (vVecT e' size) \_ -> VStar)
    let mot' = eval0 mot
    checkType i ctx base $
      mot' `vapp` vZero `vapp` vNil e'
    checkType i ctx step $
      VPi vNatT \l ->
        VPi e' \x ->
          VPi (vVecT e' l) \xs ->
            VPi (mot' `vapp` l `vapp` xs) \_ ->
              mot' `vapp` vSucc l `vapp` vCons e' l x xs
    checkType i ctx size vNatT
    let size' = eval0 size
    checkType i ctx xs (vVecT e' size')
    let tipe = (mot' `vapp` size' `vapp` eval0 xs)
    pure tipe
 where
  injNat = VExt . inject @(ExtValue ext ext) @(ValueNat ext)
  vNatT = injNat VNatT
  vZero = injNat VZero
  vSucc = injNat . VSucc
  injVec = VExt . inject @(ExtValue ext ext) @(ValueVec ext)
  vVecT e size = injVec $ VVecT e size
  vNil = injVec . VNil
  vCons e l x xs = injVec (VCons e l x xs)

substVec :: Extension ext => (TermInf ext) -> Word -> (TermVec ext) -> (TermVec ext)
substVec replacement i = \case
  Vec e size -> Vec (recur e) (recur size)
  Nil e -> Nil (recur e)
  Cons e tailSize x tail ->
    Cons (recur e) (recur tailSize) (recur x) (recur tail)
  VecElim e motive base step size xs ->
    VecElim
      (recur e)
      (recur motive)
      (recur base)
      (recur step)
      (recur size)
      (recur xs)
 where
  recur = subst replacement i
