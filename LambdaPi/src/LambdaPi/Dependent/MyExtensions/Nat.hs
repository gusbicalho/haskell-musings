{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LambdaPi.Dependent.MyExtensions.Nat where

import LambdaPi.Dependent.MyExtensions.Core

data NatExt

data TermNat ext
  = Nat
  | NatElim (TermChk ext) (TermChk ext) (TermChk ext) (TermChk ext)
  | Zero
  | Succ (TermChk ext)

deriving stock instance Eq (TermChk ext) => Eq (TermNat ext)
deriving stock instance Show (TermChk ext) => Show (TermNat ext)

data ValueNat ext
  = VNatT
  | VZero
  | VSucc (Value ext)

data NeutralNat ext
  = NNatElim (Value ext) (Value ext) (Value ext) (Neutral ext)

instance
  ( Extension extSet
  , Includes (ExtTerm extSet extSet) (TermNat extSet)
  , Includes (ExtNeutral extSet extSet) (NeutralNat extSet)
  , Includes (ExtValue extSet extSet) (ValueNat extSet)
  ) =>
  TypeExtension NatExt extSet
  where
  type ExtTerm NatExt extSet = TermNat extSet
  type ExtValue NatExt extSet = ValueNat extSet
  type ExtNeutral NatExt extSet = NeutralNat extSet
  evalExt = evalNat
  typeExt = typeNat
  substExt = substNat

instance Extension extSet => Quote (ValueNat extSet) (TermNat extSet) where
  quote = quoteNat

instance Extension extSet => Quote (NeutralNat extSet) (TermNat extSet) where
  quote = quoteNeutralNat

-- Sugar

class NatSugar term where
  nat :: TypeExtension NatExt ext => Word -> term ext
  zero :: TypeExtension NatExt ext => term ext
  suc :: TypeExtension NatExt ext => TermInf ext -> term ext

instance NatSugar TermInf where
  nat = \case
    0 -> ext Zero
    n -> ext (Succ (nat (pred n)))
  zero = ext Zero
  suc = ext . Succ . Inf

instance NatSugar TermChk where
  nat = Inf . nat
  zero = Inf zero
  suc = Inf . suc

-- Impls

evalNat ::
  forall extSet.
  TypeExtension NatExt extSet =>
  (TermNat extSet) ->
  Env extSet ->
  (Value extSet)
evalNat Nat _ = VExt (inject @_ @(ValueNat extSet) VNatT)
evalNat Zero _ = VExt (inject @_ @(ValueNat extSet) VZero)
evalNat (Succ k) env = VExt . inject $ VSucc (eval k env)
evalNat (NatElim motive base step k) env =
  let base' = eval base env
      step' = eval step env
      go = \case
        VExt (project @_ @(ValueNat extSet) -> Just VZero) -> base'
        VExt (project -> Just (VSucc j)) -> step' `vapp` j `vapp` go j
        VNeutral k -> VNeutral . NExt . inject $ NNatElim (eval motive env) base' step' k
        other -> error $ "natElim on non-Nat: " <> show (quote0 other)
   in go (eval k env)

quoteNat :: Extension ext => Word -> (ValueNat ext) -> (TermNat ext)
quoteNat n = \case
  VNatT -> Nat
  VZero -> Zero
  VSucc j -> Succ (quote n j)

quoteNeutralNat :: Extension ext => Word -> (NeutralNat ext) -> (TermNat ext)
quoteNeutralNat n = \case
  NNatElim mot base step k ->
    NatElim
      (quote n mot)
      (quote n base)
      (quote n step)
      (quote n (VNeutral k))

typeNat ::
  forall ext.
  ( TypeExtension NatExt ext
  ) =>
  Word ->
  (Context ext) ->
  (TermNat ext) ->
  Result (Value ext)
typeNat i ctx = \case
  Nat -> pure VStar
  Zero -> pure vNatT
  Succ k -> do
    checkType i ctx k vNatT
    pure vNatT
  NatElim mot base step k -> do
    checkType i ctx mot (VPi vNatT \_ -> VStar)
    let mot' = eval0 mot
    checkType i ctx base $
      mot' `vapp` vZero
    checkType i ctx step $
      VPi vNatT \l ->
        VPi (mot' `vapp` l) \_ ->
          mot' `vapp` (vSucc l)
    checkType i ctx k vNatT
    pure (mot' `vapp` eval0 k)
 where
  vExt = VExt . inject @(ExtValue ext ext) @(ValueNat ext)
  vNatT = vExt VNatT
  vZero = vExt VZero
  vSucc = vExt . VSucc

substNat :: Extension ext => TermInf ext -> Word -> TermNat ext -> TermNat ext
substNat replacement i = \case
  Nat -> Nat
  Zero -> Zero
  Succ k -> Succ (subst replacement i k)
  NatElim mot base step k ->
    NatElim
      (subst replacement i mot)
      (subst replacement i base)
      (subst replacement i step)
      (subst replacement i k)
