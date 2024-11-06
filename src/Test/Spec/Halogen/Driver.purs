module Test.Spec.Halogen.Driver where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (class MonadBracket, bracket, fork, never)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, asks, lift, runReaderT)
import Control.Monad.State (class MonadState, StateT)
import Data.Coyoneda (Coyoneda, coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda, unCoyoneda)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_, launchSuspendedAff, makeAff, suspendAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (Component, ComponentSpec, HalogenIO, HalogenM, HalogenQ(..), Slot, liftEffect, mkComponent)
import Halogen as H
import Halogen.Aff.Driver (RenderSpec)
import Halogen.Aff.Driver as AffDriver
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery (ChildQuery, ChildQueryBox, mkChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.HalogenM (mapOutput)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prim.Row (class Cons)
import Signal (Signal)
import Signal.Aff (mapAff)
import Signal.Channel (Channel, channel, send, subscribe)
import Test.Halogen.Driver (TestHalogenIO, mkTestComponent)
import Test.Spec (class Example, SpecT, after_, around, around_, before_, mapSpecTree)
import Test.Spec.Assertions (fail)
import Type.Proxy (Proxy(..))

type Settings = 
  { timeout :: Milliseconds
  }

defaultSettings :: Settings
defaultSettings = { timeout: Milliseconds 100.0 }

data AugmentedQuery :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type -> Type
data AugmentedQuery state query action slots input output a
  = Trigger action
  | ComponentQuery (query a)
  | Modify (state -> state)

data AugmentedOutput :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type
data AugmentedOutput state query action slots input output
  = Triggered action
  | Modified state
  | Raised output 

type TestComponent state query action slots input output m
  = Component
      (AugmentedQuery state query action slots input output) 
      input
      (AugmentedOutput state query action slots input output) m

type TestHalogenIO state query action slots input output m
  { query :: forall a. AugmentedQuery state query action slots input output a -> m (Maybe a)
  , messages :: Emitter (AugmentedOutput state query action slots input output)
  , dispose :: m Unit
  }

type ComponentHandle :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type
type ComponentHandle state query action slots input output = 
  { io :: TestHalogenIO state query action slots input output Aff
  , settings :: Settings
  }

mkTestComponent :: ComponentSpec state query action slots input output m -> TestComponent state query action slongs input output m
mkTestComponent spec = mkComponent (spec { eval = eval }) input
  where
    eval
      :: HalogenQ (HandleQuery state query action slots input output) action input 
      ~> HalogenM state action slots (HandleOutput state query action slots input output) Aff
    eval = case _ of
      Initialize a ->
        mapOutput Raised $ spec.eval (Initialize a)
      Finalize a ->
        mapOutput Raised $ spec.eval (Finalize a)
      Receive input a -> do
        mapOutput Raised $ spec.eval (Receive input a)
      Action action a -> do
        H.raise (Triggered action)
        mapOutput Raised $ spec.eval (Action action a)
      Query coyoneda lazy -> coyoneda # unCoyoneda \un q -> case q of
        Trigger action -> 
          mapOutput Raised $ spec.eval (Action action (lazy unit))
        ComponentQuery query ->
          mapOutput Raised $ spec.eval (Query (map un (liftCoyoneda query)) lazy)



--type TestHalogenM :: Type -> (Type -> Type) -> Type -> Row Type -> Type -> Type -> Type  -> Type
type TestHalogenM state query action slots input output a = 
  ReaderT (ComponentHandle state query action slots input output) Aff a


withComponent :: forall state query action slots input output m a. Monad m =>
  ComponentSpec state query action slots input output Aff
  -> input
  -> SpecT Aff (ComponentHandle state query action slots input output) m a
  -> SpecT Aff Unit m a
withComponent spec input =
  around (bracket runComponent (\_ -> disposeComponent))
      
    disposeComponent :: ComponentHandle state query action slots input output -> Aff Unit
    disposeComponent handle = handle.dispose
  
    runComponent :: Aff (ComponentHandle state query action slots input output) 
    runComponent = do
      { query , messages , dispose } <- runUI (mkTestComponent spec) input
      pure { query , messages , dispose , settings: defaultSettings }


data R s act ps o = R
runUI :: forall query input output
  .  Component query input output Aff
  -> input 
  -> Aff (HalogenIO query output Aff)
runUI = AffDriver.runUI renderSpec
  where
    renderSpec :: RenderSpec R
    renderSpec =
      { dispose: \_ -> pure unit
      , removeChild: \_ -> pure unit
      , render: \_ _ _ _ -> pure R
      , renderChild: identity
      }
