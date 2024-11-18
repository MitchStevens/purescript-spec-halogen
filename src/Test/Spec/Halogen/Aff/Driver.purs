module Test.Spec.Halogen.Aff.Driver where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (class MonadBracket, bracket, fork, never)
import Control.Monad.Free (liftF)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, asks, lift, local, runReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (class MonadState, StateT, get, modify, modify_)
import Control.Parallel (parSequence_)
import Data.Coyoneda (Coyoneda, coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda, unCoyoneda)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (for_, traverse_)
import Data.Lazy (Lazy)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing, maybe, maybe')
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_, launchSuspendedAff, makeAff, suspendAff, throwError, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, warn)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (Component, ComponentSpec, HalogenIO, HalogenM(..), HalogenQ(..), Request, Slot, Tell, liftEffect, mkComponent, tell, unComponent, unComponentSlot)
import Halogen as H
import Halogen.Component (ComponentSlot(..), ComponentSlotBox)
import Halogen.Data.Slot as Slot
import Halogen.HTML as HC
import Halogen.Query as HQ
import Halogen.Query.ChildQuery (ChildQueryBox, mkChildQueryBox, unChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.HalogenM (mapOutput)
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Prim.Row (class Cons)
import Test.Spec (class Example, SpecT, after_, around, around_, before_, mapSpecTree)
import Test.Spec.Assertions (fail)
import Test.Spec.Halogen.Aff.Driver.Eval as Eval
import Test.Spec.Halogen.Aff.Driver.State (DriverState(..), DriverStateRec, DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Test.Spec.Halogen.Component (AugmentedOutput, TestComponent, unTestComponent)
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

type TestHalogenIO state query action slots input output m =
  { query :: forall a. AugmentedQuery state query action slots input output a -> m (Maybe a)
  , outputs :: Emitter output
  , actions :: Emitter action
  , states :: Emitter state
  , dispose :: m Unit
  }

type ComponentHandle :: Type -> (Type -> Type) -> Type -> Row Type-> Type -> Type -> Type
type ComponentHandle state query action slots input output = 
  { io :: TestHalogenIO state query action slots input output Aff
  , settings :: Settings
  }

mkTestComponent :: forall state query action slots input output m. Eq state
  => ComponentSpec state query action slots input output m 
  -> TestComponent state query action slots input output m
mkTestComponent = unsafeCoerce


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
  { query , messages , dispose } <- runTestUI (mkTestComponent spec) input
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

type RenderSpec r =
  { render ::
      forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox ps Aff act -> Effect (RenderStateX r))
      -> HC.HTML (ComponentSlot ps Aff act) act
      -> Maybe (r s act ps o)
      -> Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

data R s act ps o = R

runTestUI
  :: forall state query action slots input output
   . TestComponent state query action slots input output Aff
  -> input
  -> Aff (TestHalogenIO state query action slots input output Aff)
runTestUI renderSpec component i = do
  lchs <- liftEffect newLifecycleHandlers
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    actionIO <- HS.create 
    stateIO <- HS.create
    outputIO <- HS.create

    driverState <-
      runComponent
        lchs
        (liftEffect <<< HS.notify stateIO.listener)
        (liftEffect <<< HS.notify actionIO.listener)
        (liftEffect <<< HS.notify outputIO.listener)
        i 
        component

    dsx <- Ref.read driverState
    dsx # unDriverStateX \st -> pure
      { query: evalDriver disposed st.selfRef
      , actions: actionIO.emitter
      , states: stateIO.emitter
      , outputs: outputIO.emitter
      , dispose: dispose disposed lchs dsx
      }
  where
  evalDriver
    :: forall s f' act ps i' o'
     . Ref Boolean
    -> Ref (DriverState r s f' act ps i' o')
    -> (forall a. f' a -> Aff (Maybe a))
  evalDriver disposed ref q =
    liftEffect (Ref.read disposed) >>=
      if _ then pure Nothing
      else Eval.evalQ render ref q

  runComponent
    :: forall s' f' act' ps' i' o'
     . Ref LifecycleHandlers
    -> (s' -> Aff Unit)
    -> (act' -> Aff Unit)
    -> (o' -> Aff Unit)
    -> i'
    -> TestComponent s' f' act' ps' i' o' Aff
    -> Effect (Ref (DriverStateX R s' act' f' o'))
  runComponent lchs stateHandler actionHandler handler j = unComponent \c -> do
    lchs' <- newLifecycleHandlers
    var <- initDriverState c j handler actionHandler stateHandler lchs'
    pre <- Ref.read lchs
    Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
    unDriverStateX (render lchs <<< _.selfRef) =<< Ref.read var
    squashChildInitializers lchs pre.initializers =<< Ref.read var
    pure var

  render
    :: forall s f' act ps i' o'
     . Ref LifecycleHandlers
    -> Ref (DriverState R s f' act ps i' o')
    -> Effect Unit
  render lchs var = Ref.read var >>= \(DriverState ds) -> do
    shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
    when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers
    Ref.write Slot.empty ds.childrenOut
    Ref.write ds.children ds.childrenIn

    let
      -- The following 3 defs are working around a capture bug, see #586
      pendingHandlers = identity ds.pendingHandlers
      pendingQueries = identity ds.pendingQueries
      selfRef = identity ds.selfRef

      handler :: Input act -> Aff Unit
      handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF render selfRef

      childHandler :: act -> Aff Unit
      childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action

    rendering <-
      renderSpec.render
        (Eval.handleAff <<< handler)
        (renderChild lchs childHandler ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering

    children <- Ref.read ds.childrenOut
    childrenIn <- Ref.read ds.childrenIn

    Slot.foreachSlot childrenIn \(DriverStateRef childVar) -> do
      childDS <- Ref.read childVar
      renderStateX_ renderSpec.removeChild childDS
      finalize lchs childDS

    flip Ref.modify_ ds.selfRef $ mapDriverState \ds' ->
      ds' { rendering = Just rendering, children = children }

    when shouldProcessHandlers do
      flip tailRecM unit \_ -> do
        handlers <- Ref.read pendingHandlers
        Ref.write (Just L.Nil) pendingHandlers
        traverse_ (Eval.handleAff <<< traverse_ fork <<< L.reverse) handlers
        mmore <- Ref.read pendingHandlers
        if maybe false L.null mmore then Ref.write Nothing pendingHandlers $> Done unit
        else pure $ Loop unit

  renderChild
    :: forall s ps act
     . Ref LifecycleHandlers
    -> (act -> Aff Unit)
    -> Ref (Slot.SlotStorage ps (DriverStateRef R s act))
    -> Ref (Slot.SlotStorage ps (DriverStateRef R s act))
    -> ComponentSlotBox ps Aff act
    -> Effect (RenderStateX R)
  renderChild lchs handler childrenInRef childrenOutRef =
    unComponentSlot \slot -> do
      childrenIn <- slot.pop <$> Ref.read childrenInRef
      var <- case childrenIn of
        Just (Tuple (DriverStateRef existing) childrenIn') -> do
          Ref.write childrenIn' childrenInRef
          dsx <- Ref.read existing
          dsx # unDriverStateX \st -> do
            flip Ref.write st.handlerRef $ maybe (pure unit) handler <<< slot.output
            Eval.handleAff $ Eval.evalM render st.selfRef (st.component.eval (HQ.Receive slot.input unit))
          pure existing
        Nothing ->
          runComponent lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
      isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
      when isDuplicate
        $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
      Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
      Ref.read var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall s' act' f' o'
     . Ref LifecycleHandlers
    -> L.List (Aff Unit)
    -> DriverStateX R s' act' f' o'
    -> Effect Unit
  squashChildInitializers lchs preInits =
    unDriverStateX \st -> do
      let parentInitializer = Eval.evalM render st.selfRef (st.component.eval (HQ.Initialize unit))
      lchs # Ref.modify_ \handlers ->
        { initializers:
            ( do
                parSequence_ (L.reverse handlers.initializers)
                parentInitializer
                liftEffect do
                  handlePending st.pendingQueries
                  handlePending st.pendingOuts
            ) : preInits
        , finalizers: handlers.finalizers
        }

  finalize
    :: forall s' act' f' o'
     . Ref LifecycleHandlers
    -> DriverStateX R s' act' f' o'
    -> Effect Unit
  finalize lchs = do
    unDriverStateX \st -> do
      cleanupSubscriptionsAndForks (DriverState st)
      let f = Eval.evalM render st.selfRef (st.component.eval (HQ.Finalize unit))
      lchs # Ref.modify_ \handlers ->
        { initializers: handlers.initializers
        , finalizers: f : handlers.finalizers
        }
      Slot.foreachSlot st.children \(DriverStateRef ref) -> do
        dsx <- Ref.read ref
        finalize lchs dsx

  dispose
    :: forall f' o'
     . Ref Boolean
    -> Ref LifecycleHandlers
    -> DriverStateX R f' o'
    -> Aff Unit
  dispose disposed lchs dsx = Eval.handleLifecycle lchs do
    Ref.read disposed >>=
      if _ then
        pure unit
      else do
        Ref.write true disposed
        finalize lchs dsx
        dsx # unDriverStateX \{ selfRef } -> do
          (DriverState ds) <- liftEffect $ Ref.read selfRef
          for_ ds.rendering renderSpec.dispose

  renderSpec :: RenderSpec R
  renderSpec =
    { dispose: \_ -> pure unit
    , removeChild: \_ -> pure unit
    , render: \_ _ _ _ -> pure R
    , renderChild: identity
    }


newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (Eval.handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall r s f act ps i o
   . DriverState r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ (traverse_ HS.unsubscribe) =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (Eval.handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks