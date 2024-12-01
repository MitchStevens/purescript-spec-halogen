module Test.Spec.Halogen.Component where

import Prelude

import Halogen (ComponentSpec)
import Unsafe.Coerce (unsafeCoerce)

data TestComponent
  (state :: Type)
  (query :: Type -> Type)
  (action :: Type)
  (slots :: Row Type)
  (input :: Type)
  (output :: Type)
  (m :: Type -> Type)


unTestComponent
  :: forall state query action slots input output m a
   . (ComponentSpec state query action slots input output m -> a)
  -> TestComponent state query action slots input output m
  -> a
unTestComponent = unsafeCoerce