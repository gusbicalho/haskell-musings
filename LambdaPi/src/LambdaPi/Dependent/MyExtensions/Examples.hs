{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}

module LambdaPi.Dependent.MyExtensions.Examples where

import Data.Void (Void)
import LambdaPi.Dependent.MyExtensions.Core
import LambdaPi.Dependent.MyExtensions.Nat
import LambdaPi.Dependent.MyExtensions.Vec

type NatVecExts = Either NatExt VecExt

id_ :: TermInf ext
id_ = Lam (Lam #_0) ~: star ~> #_0 ~> #_1

const_ :: TermInf ext
const_ = Lam (Lam (Lam (Lam #_1))) ~: star ~> star ~> #_1 ~> #_1 ~> #_3

term1 :: TermInf Void
term1 = id_ :@: #a :@: #y

term2 :: TermInf Void
term2 =
  const_
    .@ #b ~> #b
    .@ #a
    .@ (id_ .@ #b)
    .@ #y

env1 :: Extension ext => Context ext
env1 = [#y `hasType` #a, #a `hasType` Star]

env2 :: Extension ext => Context ext
env2 = [(#b `hasType` Star)] <> env1

plus :: TypeExtension NatExt ext => TermInf ext
plus =
  ( Lam . ext $ -- m
      NatElim
        (Lam $ ext Nat ~> ext Nat)
        (Lam #_0)
        ( Lam $ -- #_2 = k
            Lam $ -- #_1 = recur
              Lam $ -- #_0 = n
                suc (#_1 .@ #_0)
        )
        #_0
  )
    ~: ext Nat ~> ext Nat ~> ext Nat

append :: TermInf NatVecExts
append =
  ( Lam $ -- E
      Lam $ -- m
        Lam $ -- front :: Vec E m
          ext $
            VecElim
              #_2
              ( Lam $ -- k
                  Lam $ -- kfront
                    ext Nat -- n
                      ~> ext (Vec #_5 #_0) -- back
                      ~> ext (Vec #_6 (plus .@ #_3 .@ #_1))
              )
              ( Lam $ -- n
                  Lam $ -- back
                    #_0
              )
              ( Lam $ -- k
                  Lam $ -- x
                    Lam $ -- xs
                      Lam $ -- recur
                        Lam $ -- n
                          Lam $ -- back
                            ext (Cons #_8 (plus .@ #_5 .@ #_1) #_4 (#_2 .@ #_1 .@ #_0))
              )
              #_1
              #_0
  )
    ~: star ~> ext Nat ~> ext (Vec #_1 #_0) ~> ext Nat ~> ext (Vec #_3 #_0) ~> ext (Vec #_4 (plus .@ #_3 .@ #_1))

fromResult :: Result a -> a
fromResult = \case
  Right a -> a
  Left err -> error err

{-
>>> quote0 . fromResult . inferType0 @Void [] $ id_
Inf (Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 1)))))

>>> quote0 . fromResult . inferType0 @Void [] $ const_
Inf (Pi (Inf Star) (Inf (Pi (Inf Star) (Inf (Pi (Inf (Bound 1)) (Inf (Pi (Inf (Bound 1)) (Inf (Bound 3)))))))))

>>> quote0 (eval0 term1)
Inf (Free (Global "y"))

>>> quote0 (eval0 term2)
Lam (Inf (Bound 0))

>>> quote0 . fromResult $ inferType0 env1 term1
Inf (Free (Global "a"))

>>> quote0 . fromResult $ inferType0 env2 term2
Inf (Pi (Inf (Free (Global "b"))) (Inf (Free (Global "b"))))

>>> quote0 . fromResult $ inferType0 @NatExt [] plus
Inf (Pi (Inf (Ext Nat)) (Inf (Pi (Inf (Ext Nat)) (Inf (Ext Nat)))))

>>> quote0 . eval0 @TermInf @NatExt $ plus .@ nat 2 .@ nat 3
Inf (Ext (Succ (Inf (Ext (Succ (Inf (Ext (Succ (Inf (Ext (Succ (Inf (Ext (Succ (Inf (Ext Zero))))))))))))))))

>>> front = ext $ Cons (Inf (ext Nat)) (nat 1) (nat 0) $ Inf . ext $ Cons (Inf (ext Nat)) (nat 0) (nat 1) $ Inf . ext $ Nil (Inf (ext Nat))
>>> back = ext $ Cons (Inf (ext Nat)) (nat 0) (nat 2) $ Inf . ext $ Nil (Inf (ext Nat))
>>> x :: TermInf NatVecExts = append .@ (ext Nat) .@ nat 2 .@ front .@ nat 1 .@ back
>>> quote0 . fromResult . inferType0 [] $ x
>>> quote0 . eval0 $ x
Inf (Ext (Right (Vec (Inf (Ext (Left Nat))) (Inf (Ext (Left (Succ (Inf (Ext (Left (Succ (Inf (Ext (Left (Succ (Inf (Ext (Left Zero))))))))))))))))))
Inf (Ext (Right (Cons (Inf (Ext (Left Nat))) (Inf (Ext (Left (Succ (Inf (Ext (Left (Succ (Inf (Ext (Left Zero))))))))))) (Inf (Ext (Left Zero))) (Inf (Ext (Right (Cons (Inf (Ext (Left Nat))) (Inf (Ext (Left (Succ (Inf (Ext (Left Zero))))))) (Inf (Ext (Left (Succ (Inf (Ext (Left Zero))))))) (Inf (Ext (Right (Cons (Inf (Ext (Left Nat))) (Inf (Ext (Left Zero))) (Inf (Ext (Left (Succ (Inf (Ext (Left (Succ (Inf (Ext (Left Zero))))))))))) (Inf (Ext (Right (Nil (Inf (Ext (Left Nat))))))))))))))))))

-}
