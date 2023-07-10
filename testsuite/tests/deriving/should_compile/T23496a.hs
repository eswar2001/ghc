{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T23496a where

import Data.Kind

class C a where
  type T a :: Type

instance C Int where
  type T Int = Bool

newtype N = MkN Int
  deriving newtype C

type F :: forall a. T a -> Type
type family F a where
  F @Int True  = Float
  F @N   False = Double
