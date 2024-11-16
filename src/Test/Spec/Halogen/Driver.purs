module Test.Spec.Halogen.Driver where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (class MonadBracket, bracket, fork, never)
import Control.Monad.Free (liftF)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, asks, lift, local, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, modify, modify_)
import Data.Coyoneda (Coyoneda, coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda, unCoyoneda)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_, launchSuspendedAff, makeAff, suspendAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (Component, ComponentSpec, HalogenIO, HalogenM(..), HalogenQ(..), Request, Slot, Tell, liftEffect, mkComponent, tell)
import Halogen as H
import Halogen.Aff.Driver (RenderSpec)
import Halogen.Aff.Driver as AffDriver
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery (ChildQueryBox, mkChildQueryBox, unChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.HalogenM (mapOutput)
import Halogen.Subscription (Emitter)
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

data AugmentedQuery :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type -> Type
data AugmentedQuery state query action slots input output a
  = Trigger action
  | ComponentTell (Tell query)
  | ComponentRequest (Request query a)
  | ChildTell (Tell (ChildQueryBox slots))
  | ChildRequest (Request (ChildQueryBox slots) a)
  | Get (state -> a)
  | Modify (state -> state)

data AugmentedOutput :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type
data AugmentedOutput state query action slots input output
  = Triggered action
  | Modified state
  | Raised output 
derive instance (Eq state, Eq action, Eq output) => Eq (AugmentedOutput state query action slots input output)
instance (Show state, Show action, Show output) => Show (AugmentedOutput state query action slots input output) where
  show = case _ of
    Triggered action -> "Triggered " <> show action
    Modified state -> "Modified " <> show state
    Raised output -> "Raised " <> show output


type TestComponent state query action slots input output m
  = Component
      (AugmentedQuery state query action slots input output) 
      input
      (AugmentedOutput state query action slots input output) m

type TestHalogenIO state query action slots input output m =
  HalogenIO
    (AugmentedQuery state query action slots input output)
    (AugmentedOutput state query action slots input output)
    m


  --{ query :: forall a. AugmentedQuery state query action slots input output a -> m (Maybe a)
  --, messages :: Emitter (AugmentedOutput state query action slots input output)
  --, dispose :: m Unit
  --}

type ComponentHandle :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type
type ComponentHandle state query action slots input output = 
  { io :: TestHalogenIO state query action slots input output Aff
  , settings :: Settings
  }

mkTestComponent :: forall state query action slots input output m. Eq state
  => ComponentSpec state query action slots input output m 
  -> TestComponent state query action slots input output m
mkTestComponent spec = mkComponent (spec { eval = eval })
  where
    eval
      :: HalogenQ (AugmentedQuery state query action slots input output) action input 
      ~> HalogenM state action slots (AugmentedOutput state query action slots input output) m
    eval q = checkForStateChange $ case q of
      Initialize a ->
        mapOutput Raised $ spec.eval (Initialize a)
      Finalize a ->
        mapOutput Raised $ spec.eval (Finalize a)
      Receive input a -> do
        mapOutput Raised $ spec.eval (Receive input a)
      Action action a -> do
        H.raise (Triggered action)
        mapOutput Raised $ spec.eval (Action action a)
      Query queryCoyoneda lazy -> queryCoyoneda # unCoyoneda \un q -> case q of
        Trigger action -> do
          mapOutput Raised $ spec.eval (Action action (lazy unit))
        ComponentTell tell ->
          mapOutput Raised $ spec.eval (Query (coyoneda lazy (H.mkTell tell)) lazy)
        ComponentRequest req ->
          mapOutput Raised $ spec.eval (Query (coyoneda un (H.mkRequest req)) lazy)
        ChildTell childTell ->
          HalogenM (liftF $ H.ChildQuery (H.mkTell childTell)) $> lazy unit
        ChildRequest childRequest ->
          un <$> HalogenM (liftF $ H.ChildQuery (H.mkRequest childRequest))
        Get next -> un <<< next <$> get
        Modify f -> do
          s <- modify f
          H.raise (Modified s)
          pure (lazy unit)

    checkForStateChange ::
      HalogenM state action slots (AugmentedOutput state query action slots input output) m
      ~> HalogenM state action slots (AugmentedOutput state query action slots input output) m
    checkForStateChange halogenM = do
      s1 <- get
      res <- halogenM
      s2 <- get
      when (s1 /= s2) do
        H.raise (Modified s2)
      pure res


-- If you want this to have `MonadState TestHalogenM state` instance it needs to be wrapped in a newtype
type TestHalogenM state query action slots input output a = 
  ReaderT (ComponentHandle state query action slots input output) Aff a

setTimeout :: forall state query action slots input output a
  . TestHalogenM state query action slots input output a
  -> Milliseconds
  -> TestHalogenM state query action slots input output a
setTimeout testHalogenM timeout = 
  local (_ { settings { timeout = timeout } }) testHalogenM

withComponent :: forall state query action slots input output m a. Monad m => Eq state =>
  ComponentSpec state query action slots input output Aff
  -> input
  -> SpecT Aff (ComponentHandle state query action slots input output) m a
  -> SpecT Aff Unit m a
withComponent spec input = around (bracket (runComponent spec input) (\_ -> disposeComponent))

disposeComponent :: forall state query action slots input output.
  ComponentHandle state query action slots input output -> Aff Unit
disposeComponent handle = handle.io.dispose

runComponent :: forall state query action slots input output. Eq state 
  => ComponentSpec state query action slots input output Aff
  -> input
  -> Aff (ComponentHandle state query action slots input output)
runComponent spec input = do
  { query , messages , dispose } <- runUI (mkTestComponent spec) input
  pure { io: { query , messages , dispose}, settings: defaultSettings }

runAugmentedQuery :: forall state query action slots input output a.
  AugmentedQuery state query action slots input output a
  -> TestHalogenM state query action slots input output (Maybe a)
runAugmentedQuery q = do
  (query :: forall a. AugmentedQuery state query action slots input output a -> Aff (Maybe a)) <- asks (_.io.query)
  liftAff $ query q




getComponentState :: forall state query action slots input output.
  TestHalogenM state query action slots input output state
getComponentState = do
  maybeState <- runAugmentedQuery (Get identity)
  maybe' (\_ -> throwError (error "couldn't get State! Should never happen!")) pure maybeState

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