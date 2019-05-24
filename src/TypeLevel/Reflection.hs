{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module TypeLevel.Reflection (
  TypeName
  ) where

  import GHC.TypeLits
  import GHC.Generics
  import Data.Type.Bool

  -- Type -> Symbol
  -- from http://www.mchaver.com/posts/2017-12-12-type-name-to-string.html
  type family TypeName a :: Symbol where
    TypeName Double = "Double"
    TypeName Int = "Int"
    TypeName String = "String"
    TypeName (M1 D ('MetaData name _ _ _) f ()) = name
    TypeName a = TypeName (Rep a ())
