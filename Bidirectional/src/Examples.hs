module Examples where
import Bidirectional.Language
import Bidirectional qualified
import BidirectionalWithImplicitCtx qualified

test_id :: Expr
test_id = ELam "x" (EVar (NamedVar "x"))

test_call_id :: Expr
test_call_id = EApply (ELam "f" (EApply (EVar (NamedVar "f")) EUnit)) test_id

test_anno_id :: Expr
test_anno_id = EAnno test_id (TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T"))))

test_const :: Expr
test_const = ELam "x" (ELam "y" (EVar (NamedVar "x")))

test_constUnit :: Expr
test_constUnit = EApply test_const EUnit

test_always_unit :: Expr
test_always_unit = ELam "x" EUnit

-- Helper

expectRight :: Either String c -> c
expectRight = either error id

testBidirectionalWithImplicitCtx :: Expr -> Tipe
testBidirectionalWithImplicitCtx = expectRight . BidirectionalWithImplicitCtx.typeComplete

testBidirectional :: Expr -> Tipe
testBidirectional = expectRight . Bidirectional.typeComplete

{-

--------------------------------------------------------------------------------
Bidirectional

>>> testBidirectional $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> testBidirectional $ test_call_id
TUnit

>>> testBidirectional $ test_anno_id
TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> testBidirectional $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> testBidirectional $ test_always_unit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> testBidirectional $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

--------------------------------------------------------------------------------
BidirectionalWithImplicitCtx

>>> testBidirectionalWithImplicitCtx $ test_id
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TVar (FreshVar "->I\8658_arg" 0))

>>> testBidirectionalWithImplicitCtx $ test_call_id
TUnit

>>> testBidirectionalWithImplicitCtx $ test_anno_id
TForall "T" (TFunction (TVar (NamedVar "T")) (TVar (NamedVar "T")))

>>> testBidirectionalWithImplicitCtx $ test_const
TFunction (TVar (FreshVar "->I\8658_arg" 0)) (TFunction (TVar (FreshVar "InstRArr_arg" 4)) (TVar (FreshVar "->I\8658_arg" 0)))

>>> testBidirectionalWithImplicitCtx $ test_always_unit
TFunction (TVar (FreshVar "->I\8658_arg" 0)) TUnit

>>> testBidirectionalWithImplicitCtx $ test_constUnit
TFunction (TVar (FreshVar "InstRArr_arg" 4)) TUnit

-}
