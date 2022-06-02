module Examples where
import Bidirectional.Language
import Bidirectional qualified
import BidirectionalWithImplicitCtx qualified
import BidirectionalWithPolyLam qualified

test_id :: Expr
test_id = ELam "x" (EVar (NamedVar "x"))

test_call_id :: Expr
test_call_id = EApply (ELam "f" (EApply (EVar (NamedVar "f")) EUnit)) test_id

test_anno_id :: Expr
test_anno_id = EAnno test_id (TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T"))))

test_const :: Expr
test_const = ELam "x" (ELam "y" (EVar (NamedVar "x")))

test_constUnit :: Expr
test_constUnit = EApply test_const EUnit

test_always_unit :: Expr
test_always_unit = ELam "x" EUnit

test_const_higher_rank :: Expr
test_const_higher_rank =
  EAnno
    test_const
    (TForall
      (NamedVar "T")
      (TFunction
        (TVar (NamedVar "T"))
        (TForall
          (NamedVar "U")
          (TFunction
            (TVar (NamedVar "U"))
            (TVar (NamedVar "T"))))))

test_const_higher_rank_unit :: Expr
test_const_higher_rank_unit = EApply test_const_higher_rank EUnit

-- Helper

expectRight :: Either String c -> c
expectRight = either error id

testBidirectional :: Expr -> Tipe
testBidirectional = expectRight . Bidirectional.typeComplete

testBidirectionalWithImplicitCtx :: Expr -> Tipe
testBidirectionalWithImplicitCtx = expectRight . BidirectionalWithImplicitCtx.typeComplete

testBidirectionalWithPolyLam :: Expr -> Tipe
testBidirectionalWithPolyLam = expectRight . BidirectionalWithPolyLam.typeComplete

{-

--------------------------------------------------------------------------------
Bidirectional

>>> testBidirectional $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> testBidirectional $ test_call_id
TUnit

>>> testBidirectional $ test_anno_id
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> testBidirectional $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> testBidirectional $ test_always_unit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> testBidirectional $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

>>> testBidirectional $ test_const_higher_rank
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) (TVar (NamedVar "T")))))

>>> testBidirectional $ test_const_higher_rank_unit
TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) TUnit)

--------------------------------------------------------------------------------
BidirectionalWithImplicitCtx

>>> testBidirectionalWithImplicitCtx $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> testBidirectionalWithImplicitCtx $ test_call_id
TUnit

>>> testBidirectionalWithImplicitCtx $ test_anno_id
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> testBidirectionalWithImplicitCtx $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> testBidirectionalWithImplicitCtx $ test_always_unit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> testBidirectionalWithImplicitCtx $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

>>> testBidirectionalWithImplicitCtx $ test_const_higher_rank
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) (TVar (NamedVar "T")))))

>>> testBidirectionalWithImplicitCtx $ test_const_higher_rank_unit
TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) TUnit)

--------------------------------------------------------------------------------
BidirectionalWithPolyLam

>>> testBidirectionalWithPolyLam $ test_id
TForall (FreshVar "->I\8658_arg" 0) (TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> testBidirectionalWithPolyLam $ test_call_id
TUnit

>>> testBidirectionalWithPolyLam $ test_anno_id
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> testBidirectionalWithPolyLam $ test_const
TForall (FreshVar "->I\8658_arg" 0) (TForall (FreshVar "InstRArr_arg" 4) (TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))))

>>> testBidirectionalWithPolyLam $ test_always_unit
TForall (FreshVar "->I\8658_arg" 0) (TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit)

>>> testBidirectionalWithPolyLam $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

>>> testBidirectionalWithPolyLam $ test_const_higher_rank
TForall (NamedVar "T") (TFunction (TVar (NamedVar "T")) (TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) (TVar (NamedVar "T")))))

>>> testBidirectionalWithPolyLam $ test_const_higher_rank_unit
TForall (NamedVar "U") (TFunction (TVar (NamedVar "U")) TUnit)

-}
