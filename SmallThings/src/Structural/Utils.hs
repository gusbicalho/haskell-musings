{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Structural.Utils where

import Data.Type.Ord (Compare, OrdCond)
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

type family ConstructorName (t :: G.Meta) where
  ConstructorName ( 'G.MetaCons name _ _) = name
  ConstructorName meta =
    TypeError
      ( 'Text "ConstructorName error"
          ':$$: 'ShowType meta
          ':$$: 'Text "is not a MetaCons"
      )

type IsSameSymbol (s1 :: Symbol) (s2 :: Symbol) = OrdCond (Compare s1 s2) 'False 'True 'False

-- | Compile-time if
class TyIf (cond :: Bool) where
  tyIf :: t -> t -> t

instance TyIf 'False where
  tyIf _ e = e

instance TyIf 'True where
  tyIf t _ = t
