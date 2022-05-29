{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Henk.Example where

import Henk
import Henk.Sugar

pattern EType :: Expression id lit
pattern EType = EK TYPE

pattern EKind :: Expression id lit
pattern EKind = EK KIND

pattern ENat :: Expression id lit
pattern ENat = EK NAT

expectRight :: Either String b -> b
expectRight = \case
  Right x -> x
  Left err -> error err

test :: Result (Ctx String Word)
test = checkProgram (const ENat) [] example

-- >>> expectRight test
-- Unable to reduce argument type to WHNF:
--   EApply (ELookup (Var "list" :~ EPi (Ignore :~ EK TYPE) (EK TYPE))) (EK NAT)

example :: Program String Word
example =
  let tree = #tree ~: (EType ~> EType) ~> EType ~> EType
      branch =
        #branch
          ~: let m = #m ~: EType ~> EType
                 a = #a ~: EType
              in pi'
                  [m, a]
                  (a ~> m ! (tree ! m ! a) ~> tree ! m ! a)
      list = #list ~: EType ~> EType
      nil =
        #nil
          ~: let e = #nile ~: EType
              in pi' [e] (list ! e)
      cons =
        #cons
          ~: let e = #conse ~: EType
              in pi' [e] (e ~> (list ! e) ~> list ! e)
      numbers = #numbers ~: tree ! list ! ENat
      -- plus = #plus ~: ENat ~> ENat ~> ENat
      -- sumTree = #sumTree ~: tree ! list ! ENat ~> ENat
      -- sumList = #sumList ~: list ! (tree ! list ! ENat) ~> ENat
      -- main = #main ~: ENat
   in MkProgram
        [ tree ::= [branch]
        , list ::= [nil, cons]
        ]
        [ numbers
            =: branch ! list ! ENat
              ! lit 4
              ! ( cons ! ENat
                    ! (branch ! list ! ENat ! lit 2 ! (nil ! ENat))
                    ! ( cons ! ENat
                          ! (branch ! list ! ENat ! lit 2 ! (nil ! ENat))
                          ! (nil ! ENat)
                      )
                )
        -- , letrec
        --     [ sumTree
        --         =: let arg = #sumTreeArg ~: tree ! list ! ENat
        --             in arg
        --                 --> caseOf
        --                   arg
        --                   [ branch
        --                       ==> let v = #sumTreeBranchV ~: ENat
        --                               children = #children ~: list ! (tree ! list ! ENat)
        --                            in v --> children --> plus ! v ! (sumList ! children)
        --                   ]
        --                   `at` [toExpr list, toExpr ENat]
        --     , sumList
        --         =: let arg = #sumListArg ~: list ! (tree ! list ! ENat)
        --             in arg
        --                 --> caseOf
        --                   arg
        --                   [ nil ==> lit 0
        --                   , cons
        --                       ==> let car1 = #car1 ~: tree ! list ! ENat
        --                               cdr1 = #cdr1 ~: list ! (tree ! list ! ENat)
        --                            in plus ! (sumTree ! car1) ! (sumList ! cdr1)
        --                   ]
        --                   `at` [tree ! list ! ENat]
        --     ]
        -- , main =: sumTree ! numbers
        ]
