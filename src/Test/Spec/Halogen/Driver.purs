module Test.Spec.Halogen.Driver where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (class MonadBracket, bracket, fork, never)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, asks, lift, runReaderT)
import Control.Monad.State (class MonadState, StateT)
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
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
import Halogen (Component, ComponentSpec, HalogenIO, HalogenQ(..), Slot, liftEffect, mkComponent)
import Halogen.Aff.Driver (RenderSpec)
import Halogen.Aff.Driver as AffDriver
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery (ChildQuery, ChildQueryBox, mkChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prim.Row (class Cons)
import Signal (Signal)
import Signal.Aff (mapAff)
import Signal.Channel (Channel, channel, send, subscribe)
import Test.Spec (class Example, SpecT, after_, around, around_, before_, mapSpecTree)
import Test.Spec.Assertions (fail)
import Type.Proxy (Proxy(..))

type Settings = 
  { timeout :: Milliseconds
  }

defaultSettings :: Settings
defaultSettings = { timeout: Milliseconds 100.0 }

type ComponentHandle state query action slots input output  = 
  { stateChannel :: Channel state
  , querySignal :: Signal (Maybe (Exists (Coyoneda query)))
  , actionChannel :: Channel (Maybe action)
  , inputChannel :: Channel input
  , outputSignal :: Signal (Maybe output)
  , query :: forall a. query a -> Aff (Maybe a)
  , settings :: Settings
  }
    

type TestHalogenM :: Type -> (Type -> Type) -> Type -> Row Type -> Type -> Type -> Type  -> Type
type TestHalogenM state query action slots input output a =
  ReaderT (ComponentHandle state query action slots input output) Aff a

withComponent :: forall state query action slots input output m a. Monad m =>
  ComponentSpec state query action slots input output Aff
  -> input
  -> SpecT Aff (ComponentHandle state query action slots input output) m a
  -> SpecT Aff Unit m a
withComponent spec input =
  around (bracket runComponent (\_ -> disposeComponent))

  where
    disposeComponent :: ComponentHandle state query action slots input output -> Aff Unit
    disposeComponent handle = handle.io.dispose

    runComponent :: Aff (ComponentHandle state query action slots input output)
    runComponent = do
      stateChannel <- liftEffect $ channel (spec.initialState input)
      queryChannel <- liftEffect $ channel Nothing
      actionChannel <- liftEffect $ channel Nothing
      inputChannel <- liftEffect $ channel input
      outputChannel <- liftEffect $ channel Nothing

      let eval = case _ of
                  Initialize a ->
                    spec.eval (Initialize a)
                  Finalize a ->
                    spec.eval (Finalize a)
                  Receive input a -> do
                    send inputChannel input
                    spec.eval (Receive input a)
                  Action action a -> do
                    send actionChannel (Just action)
                    spec.eval (Action action a)
                  Query coyoneda lazy -> do
                    send queryChannel (Just (mkExists coyoneda))
                    spec.eval (Query coyoneda lazy)

      io <- runUI (mkComponent (spec { eval = eval })) input
      HS.subscribe io.messages (send outputChannel)
      sendQuery <- liftEffect $ mapAff $ runExists $ unCoyoneda \f query -> map (map f) $ io.query query
      subscribe queryChannel 

      pure
        { stateChannel
        , actionChannel
        , inputChannel
        , querySignal: subscribe queryChannel
        , outputSignal: subscribe outputChannel
        , query: io.query
        , settings: defaultSettings 
        }


data R s act ps o = R
runUI :: forall state query action slots input output
  .  Component state query action slots input output Aff
  -> input 
  -> Aff (HalogenIO state query Aff)
runUI = AffDriver.runUI renderSpec
  where
    renderSpec :: RenderSpec R
    renderSpec =
      { dispose: \_ -> pure unit
      , removeChild: \_ -> pure unit
      , render: \_ _ _ _ -> pure R
      , renderChild: identity
      }




tell :: forall state query action output slots.
  (forall x. x -> query x) 
  -> TestHalogenM state query action output slots Unit
tell q = do
  queryChannel <- asks (_.queryChannel)
  liftEffect $ send queryChannel (Just (q unit))

request :: forall state query action output slots a.
  query a -> TestHalogenM state query action output slots (Maybe a)
request q = do
  queryChannel <- asks (_.queryChannel)
  liftEffect $ send queryChannel (Just ())

trigger :: forall state query action output slots.
  action -> TestHalogenM state query action output slots Unit
trigger action = do
  (io :: TestHalogenIO state query action slots output) <- asks (_.io)
  void $ liftAff (io.query (ComponentAction action))

componentState :: forall state query action output slots. TestHalogenM state query action output slots state
componentState = do
  (io :: TestHalogenIO state query action slots output) <- asks (_.io)
  maybeState <- lift (io.query (GetState identity))
  maybe (throwError (error "couldn't get state of component")) pure maybeState

childTell :: forall state query action output slots label _1 q o slot
  .  Cons label (Slot q o slot) _1 slots 
  => IsSymbol label 
  => Ord slot 
  => Proxy label
  -> slot 
  -> (forall x. x -> q x)
  -> TestHalogenM state query action output slots Unit
childTell label slot q = do
  (io :: TestHalogenIO state query action slots output) <- asks (_.io)
  void $ lift (io.query (QueryChild childQueryBox))
  where
    childQueryBox = mkChildQueryBox $
      CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label slot) (q unit) identity

