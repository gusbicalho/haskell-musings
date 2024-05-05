{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hanoi () where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint, Type)
import Data.Type.Equality ((:~:) (Refl))
import Prelude hiding (LT)

type Dict :: Constraint -> Type
data Dict c where
  MkDict :: (c) => Dict c

data Pos where
  One :: Pos
  S :: Pos -> Pos

type Two = S One

type SNat :: Pos -> Type
data SNat n where
  SOne :: SNat One
  SS :: !(SNat n) -> SNat (S n)

pattern STwo :: () => (n ~ S n1, n1 ~ One) => SNat n
pattern STwo = SS SOne

type Add :: Pos -> Pos -> Pos
type family Add a b where
  Add One b = (S b)
  Add (S a') b = Add a' (S b)

type (<) :: Pos -> Pos -> Type
data a < b where
  LT :: forall a b diff. ((Add diff a) ~ b) => SNat a -> SNat diff -> SNat b -> a < b

type (++) :: [Pos] -> [Pos] -> [Pos]
type family as ++ bs where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

type Tower :: Type -> [Pos] -> Type
data Tower id ns where
  Empty :: !id -> Tower id '[]
  First :: SNat n -> Tower id '[] -> Tower id '[n]
  More :: SNat n -> n < top -> Tower id (top : discs) -> Tower id (n : top : discs)

popTower :: (discs ~ (a : b : more)) => Tower id discs -> (Tower id (Popped discs), Tower id '[(Last discs)])
popTower (More a _ltA (First b empty)) = (First a empty, First b empty)
popTower (More a ltA (More b ltB more)) = case popTower (More b ltB more) of
  (more, last) -> (More a ltA more, last)

data Hanoi a as b bs c cs where
  H :: Tower a as -> Tower b bs -> Tower c cs -> Hanoi a as b bs c cs

type CanReceive :: [Pos] -> Pos -> Type
data discs `CanReceive` top where
  IsEmpty :: forall top discs. (discs ~ '[]) => discs `CanReceive` top
  IsLarger :: forall top discs top' more. (discs ~ (top' : more)) => top < top' -> discs `CanReceive` top

type ProveForAll :: forall k. (k -> Type) -> [k] -> Type
data ProveForAll prover elems where
  Vacuous :: ProveForAll prover '[]
  ProveForOne :: prover one -> ProveForAll prover '[one]
  ProveForMore :: prover one -> ProveForAll prover (two : more) -> ProveForAll prover (one : two : more)

class AllForOne prover elems elem where
  allForOne :: ProveForAll prover elems -> prover elem

instance {-# OVERLAPPING #-} AllForOne prover (disc : more) disc where
  allForOne (ProveForOne proof) = proof
  allForOne (ProveForMore proof _) = proof

instance {-# OVERLAPPABLE #-} (AllForOne prover more disc, more ~ (two : many)) => AllForOne prover (any : more) disc where
  allForOne (ProveForMore _ more) = allForOne more

type Popped :: forall k. [k] -> [k]
type family Popped xs where
  Popped '[x] = '[]
  Popped (x : more) = x : Popped more

type Last :: forall k. [k] -> k
type family Last xs where
  Last '[x] = x
  Last (_ : more) = Last more

popProof :: (elems ~ (a : b : more)) => ProveForAll prover elems -> (ProveForAll prover (Popped elems), prover (Last elems))
popProof (ProveForMore a (ProveForOne b)) = (ProveForOne a, b)
popProof (ProveForMore a (ProveForMore b more)) = case popProof (ProveForMore b more) of
  (more, last) -> (ProveForMore a more, last)

-- instance AllForOneAllForOne (CanReceive discs) '[disc] disc where
--   allForOne (ProveForOne proof) = proof

putDisc :: (discs `CanReceive` top) -> SNat top -> Tower x discs -> Tower x (top : discs)
putDisc IsEmpty top tower = First top tower
putDisc (IsLarger topLT) top tower = More top topLT tower

data RemovedDiscWasSmallest removed leftover where
  LeftEmpty :: forall removed leftover. (leftover ~ '[]) => RemovedDiscWasSmallest removed leftover
  LeftLarger :: forall removed leftover top more. (leftover ~ (top : more)) => removed < top -> RemovedDiscWasSmallest removed leftover

moveTop :: (bs `CanReceive` top) -> Tower a (top : as) -> Tower b bs -> (Tower a as, Tower b (top : bs), RemovedDiscWasSmallest top as)
moveTop canReceive ta tb = case ta of
  First top empty -> (empty, (putDisc canReceive top tb), LeftEmpty)
  More top lt more -> (more, (putDisc canReceive top tb), LeftLarger lt)

one :: Hanoi a (One : discs) b '[] c '[] -> Hanoi a discs b '[One] c '[]
one (H ta tb tc) = case moveTop IsEmpty ta tb of
  (ta, tb, _) -> H ta tb tc

-- two :: Hanoi a (One : Two : discs) b '[] c '[] -> Hanoi a discs b '[] c '[One, Two]
two :: Hanoi a (top1 : top2 : as) b bs c cs -> bs `CanReceive` top1 -> cs `CanReceive` top2 -> Hanoi a as b bs c (top1 : top2 : cs)
two (H ta tb tc) bCanReceive cCanReceive = runIdentity $ do
  (ta, tb, lt1) <- pure $ moveTop bCanReceive ta tb
  (ta, tc, _lt2) <- pure $ moveTop cCanReceive ta tc
  let bCanReceiveFromC = IsLarger $ case lt1 of
        LeftLarger lt -> lt
  (tb, tc, _) <- pure $ moveTop bCanReceiveFromC tb tc
  pure $ H ta tb tc

emptyCanReceiveAll :: (xs ~ (_a : _more)) => Tower x xs -> ProveForAll (CanReceive '[]) xs
emptyCanReceiveAll (First _x _empty) = ProveForOne IsEmpty
emptyCanReceiveAll (More _ _ more) = ProveForMore IsEmpty (emptyCanReceiveAll more)

-- solve ::
--   forall a as b bs c cs.
--   ProveForAll (CanReceive bs) as ->
--   ProveForAll (CanReceive cs) as ->
--   ProveForAll (CanReceive bs) cs ->
--   Hanoi a as b bs c cs ->
--   Either (Hanoi a '[] b bs c (as ++ cs)) (Hanoi a '[] b (as ++ bs) c cs)
-- solve bCanReceiveA cCanReceiveA bCanReceiveC (H ta tb tc) = case ta of
--   Empty _ -> Right $ H ta tb tc
--   First top empty -> Right $ H empty (putDisc (allForOne bCanReceiveA) top tb) tc
--   More _ _ _ -> runIdentity do
--     (top, last) <- pure $ popTower ta
--     (bCanReceiveTop, bCanReceiveLast) <- pure $ popProof bCanReceiveA
--     (cCanReceiveTop, cCanReceiveLast) <- pure $ popProof cCanReceiveA
--     case solve bCanReceiveTop cCanReceiveTop bCanReceiveC (H top tb tc) of
--       Right (H top tb tc) -> case last of
--         First lastDisc emptyA -> case putDisc cCanReceiveLast lastDisc tc of
--           !tc -> runIdentity do
--             let h = (H tb tc emptyA)
--             _
--       Left (H top tb tc) -> runIdentity do
--         let First lastDisc emptyA = last
--         _

-- H tc tb _ <- pure $ solve bCanReceiveC (emptyCanReceiveAll tc) Vacuous (H tc tb (Empty ()))
-- tb <- pure $ putDisc (bCanReceiveLast) last tb
-- _

-- case (bCanReceive, cCanReceive) of
--   (ProveForOne _bCanReceive, ProveForOne cCanReceive) -> case ta of
--     First top emptyA -> H emptyA tb (putDisc cCanReceive top tc)
--     More top _lt moreA -> H moreA tb (putDisc cCanReceive top tc)
--   (ProveForMore _ _, ProveForMore _ _) -> runIdentity do
--     (bCanReceiveTop, bCanReceiveLast) <- pure $ popProof bCanReceive
--     (cCanReceiveTop, cCanReceiveLast) <- pure $ popProof cCanReceive
--     (top, last) <- pure $ popTower ta
--     h2 <- pure $ solve bCanReceiveTop cCanReceiveTop (H top tb tc)
--     _

-- case ta of
--   Empty _ -> (H ta tb tc, ())
--   First top emptyA -> do
--     (H emptyA tb (putDisc (allForOne cCanReceive) top tc), ())
--   More top lt (moreA :: Tower top2 moreAs) -> runIdentity $ do
--     let (ProveForMore _ bCanReceiveMore) = bCanReceive
--     let (ProveForMore _ cCanReceiveMore) = cCanReceive
--     (H moreA tb tc, ()) <- pure $ solve bCanReceiveMore cCanReceiveMore (H moreA tb tc)
--     let ta = First
--     -- let (H ta tb tc, _) = moveTop
--     _

-- solve :: Hanoi a discs b '[] c '[] -> Either (Hanoi a '[] b)
