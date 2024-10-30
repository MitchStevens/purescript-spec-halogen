module Test.Spec.Halogen.Driver where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (class MonadBracket, bracket, fork, never)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, asks, lift, runReaderT)
import Control.Monad.State (class MonadState, StateT)
import Control.Monad.State as State
import Data.Coyoneda (Coyoneda, coyoneda, unCoyoneda)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.Lazy (Lazy)
import Data.Lens (Prism', prism)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_, launchSuspendedAff, makeAff, suspendAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (Component, ComponentSpec, HalogenIO, HalogenM, HalogenQ(..), Slot, liftEffect, mkComponent)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Driver (RenderSpec)
import Halogen.Aff.Driver as AffDriver
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery (ChildQuery, ChildQueryBox, mkChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Subscription (Emitter, Listener, SubscribeIO, notify)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prim.Row (class Cons)
import Test.Spec (class Example, SpecT, after_, around, around_, before_, mapSpecTree)
import Test.Spec.Assertions (fail)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Settings = 
  { timeout :: Milliseconds
  }

defaultSettings :: Settings
defaultSettings = { timeout: Milliseconds 100.0 }

data TestQuery state query action slots input output
  = Received input
  | Trigger action
  | ComponentQuery (forall x. x -> query x)
  -- | childQuery
  | Modify (state -> state)

data TestOutput state query action slots input output
  = Modified state
  | Queried (query Unit)
  | Triggered action
  | Raised output

{-

modified :: Prism' (TestOutput state query action slots input output) state
modified = prism Modified $ case _ of
  Modified state -> Right state
  other -> Left other

queried :: Prism' (TestOutput state query action slots input output) (query Unit)
queried = prism Queried $ case _ of
  Queried queried -> Right queried
  other -> Left other

triggered :: Prism' (TestOutput state query action slots input output) action
triggered = prism Triggered $ case _ of
  Triggered action -> Right action
  other -> Left other

raised :: Prism' (TestOutput state query action slots input output) output
raised = prism Raised $ case _ of
  Raised output -> Right output
  other -> Left other


type ComponentHandle state query action slots input output  = 
  { state :: SubscribeIO state
  , action :: SubscribeIO action
  , input :: Listener input
  , query :: Listener (forall x. query x)
  , output :: Emitter output

  , settings :: Settings
  , dispose :: Aff Unit
  }
    

type TestHalogenM :: Type -> (Type -> Type) -> Type -> Row Type -> Type -> Type -> Type  -> Type
newtype TestHalogenM state query action slots input output a = TestHalogenM
  (ReaderT (ComponentHandle state query action slots input output) Aff a)


runTestHalogenM :: forall state query action slots input output a.
  TestComponent state query action slots input output a
  -> ComponentHandle state query action slots input output
  -> Aff a

withComponent :: forall state query action slots input output m a. Monad m => Eq state
  => ComponentSpec state query action slots input output Aff
  -> input
  -> SpecT Aff (ComponentHandle state query action slots input output) m a
  -> SpecT Aff Unit m a
withComponent spec componentInput =
  around (bracket runComponent (\_ -> _.dispose))

  where
    runComponent :: Aff (ComponentHandle state query action slots input output)
    runComponent = do

      let
        checkForChangedState :: HalogenM state action slots output Aff ~> HalogenM state action slots output Aff
        checkForChangedState action = do
          state <- State.get
          res <- action
          state' <- State.get
          when (state /= state') do
            notify stateOut.listener state'
          pure res

      let 
        eval :: HalogenQ query action input ~> HalogenM state action slots output Aff
        eval q = checkForChangedState $ case q of
          Initialize a -> do
            _ <- HS.subscribe
            H.subsc
            _ <- HS.subscribe actionIn.emitter \action -> eval (action a)
            spec.eval (Initialize a)
          Finalize a ->
            spec.eval (Finalize a)
          Receive input a -> do
            spec.eval (Receive input a)
          Action action a -> do
            spec.eval (Action action a)
            notify actionOut.listener action
          Query coyoneda lazy -> do
            spec.eval (Query coyoneda lazy)

      _ <- HS.subscribe actionIn.emitter \state ->
        runHalogenAff


      forSignal_ (subscribe stateChannel) \state -> do
        runHalogenAff (State.put state)

(HalogenQ query action input) ~> (HalogenM state action slots output m)




      io <- runUI (mkComponent (spec { eval = eval })) componentInput
      _ <- liftEffect $ HS.subscribe io.messages (\o -> send outputChannel (Just o))
      --sendQuery <- liftEffect $ mapAff $ runExists $ unCoyoneda \f query -> map (map f) $ io.query query
      --subscribe queryChannel 

      pure
        { stateChannel
        , actionChannel
        , inputChannel
        , stateSignal: subscribe stateChannel
        , querySignal: subscribe queryChannel
        , outputSignal: subscribe outputChannel
        , query: unsafeCoerce io.query -- why do i need unsafecoerce here?
        , settings: defaultSettings 
        , dispose: io.dispose
        }


data R s act ps o = R
runUI :: forall state query action slots input output
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




tell :: forall state query action slots input output.
  (forall x. x -> query x) 
  -> TestHalogenM state query action slots input output Unit
tell q = do
  (query :: forall a. query a -> Aff (Maybe a)) <- asks (_.query)
  void $ liftAff $ query (q unit)

request :: forall state query action slots input output a.
  query a -> TestHalogenM state query action slots input output (Maybe a)
request q = do
  (query :: forall a. query a -> Aff (Maybe a)) <- asks (_.query)
  liftAff $ query q

trigger :: forall state query action slots input output.
  action -> TestHalogenM state query action slots input output Unit
trigger action = do
  channel <- asks (_.actionChannel)
  liftEffect $ send channel (Just action)

-}
--componentState :: forall state query action output slots. TestHalogenM state query action output slots state
--componentState = do
--  (io :: TestHalogenIO state query action slots output) <- asks (_.io)
--  maybeState <- lift (io.query (GetState identity))
--  maybe (throwError (error "couldn't get state of component")) pure maybeState
--
--childTell :: forall state query action output slots label _1 q o slot
--  .  Cons label (Slot q o slot) _1 slots 
--  => IsSymbol label 
--  => Ord slot 
--  => Proxy label
--  -> slot 
--  -> (forall x. x -> q x)
--  -> TestHalogenM state query action output slots Unit
--childTell label slot q = do
--  (io :: TestHalogenIO state query action slots output) <- asks (_.io)
--  void $ lift (io.query (QueryChild childQueryBox))
--  where
--    childQueryBox = mkChildQueryBox $
--      CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label slot) (q unit) identity

