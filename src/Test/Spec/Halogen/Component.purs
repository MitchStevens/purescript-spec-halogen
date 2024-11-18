module Test.Spec.Halogen.Component where

import Prelude

import Halogen (ComponentSpec)
import Unsafe.Coerce (unsafeCoerce)

data AugmentedOutput :: Type -> Type -> Type -> Type
data AugmentedOutput state action output
  = Modified state
  | Triggered action
  | Raised output 
derive instance (Eq state, Eq action, Eq output) => Eq (AugmentedOutput state action output)
instance (Show state, Show action, Show output) => Show (AugmentedOutput state action output) where
  show = case _ of
    Triggered action -> "Triggered " <> show action
    Modified state -> "Modified " <> show state
    Raised output -> "Raised " <> show output

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