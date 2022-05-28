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

test :: Result (Ctx String Word)
test = checkProgram (const . ELookup $ #nat ~: EType) [("nat", EType)] example

-- >>> test
-- Left "Non-PI type in application: EK TYPE"

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
          ~: let e = #e ~: EType
              in pi' [e] (list ! e)
      cons =
        #cons
          ~: let e = #e ~: EType
              in pi' [e] (e ~> (list ! e) ~> list ! e)
      nat = #nat ~: EType
      numbers = #numbers ~: tree ! list ! nat
      plus = #plus ~: nat ~> nat ~> nat
      sumTree = #sumTree ~: tree ! list ! nat ~> nat
      sumList = #sumList ~: list ! (tree ! list ! nat) ~> nat
      main = #main ~: nat
   in MkProgram
        [ tree ::= [branch]
        , list ::= [nil, cons]
        ]
        [ letrec
            [ sumTree
                =: let arg = #arg ~: tree ! list ! nat
                    in arg
                        --> caseOf
                          arg
                          [ branch
                              ==> let v = #v ~: nat
                                      children = #children ~: list ! (tree ! list ! nat)
                                   in v --> children --> plus ! v ! (sumList ! children)
                          ]
                          `at` [toExpr list, toExpr nat]
            , sumList
                =: let arg = #arg ~: list ! (tree ! list ! nat)
                    in arg
                        --> caseOf
                          arg
                          [ nil ==> lit 0
                          , cons
                              ==> let car1 = #car1 ~: tree ! list ! nat
                                      cdr1 = #cdr1 ~: list ! (tree ! list ! nat)
                                   in plus ! (sumTree ! car1) ! (sumList ! cdr1)
                          ]
                          `at` [tree ! list ! nat]
            ]
        , numbers
            =: branch ! list ! nat
              ! lit 4
              ! ( cons ! nat
                    ! (branch ! list ! nat ! lit 2 ! (nil ! nat))
                    ! ( cons ! nat
                          ! (branch ! list ! nat ! lit 2 ! (nil ! nat))
                          ! (nil ! nat)
                      )
                )
        , main =: sumTree ! numbers
        ]
