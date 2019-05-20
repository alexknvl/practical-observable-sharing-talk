module Unsafe.StableName (StableName, makeStableName) where

import Prelude (class Eq)
import Effect (Effect)
import Data.Hashable (class Hashable)

foreign import data StableName :: Type -> Type

foreign import makeStableName :: forall a. a -> Effect (StableName a)

foreign import unsafeNameEq :: forall a. StableName a -> StableName a -> Boolean

foreign import unsafeNameHash :: forall a. StableName a -> Int

instance stableNameEq :: Eq (StableName a) where
  eq a b = unsafeNameEq a b

instance stableNameHashable :: Hashable (StableName a) where
  hash a = unsafeNameHash a