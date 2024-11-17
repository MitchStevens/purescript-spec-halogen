module Test.Spec.Halogen.Assertions where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Except (ExceptT, lift, throwError)
import Control.Monad.Fork.Class (bracket, fork, kill, never, suspend)
import Control.Monad.Reader (asks)
import Data.Array (cons)
import Data.Array as A
import Data.Foldable (class Foldable, for_)
import Data.HeytingAlgebra (ff)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, joinFiber, killFiber, launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (Request, Slot, Tell, mkRequest, mkTell)
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery (ChildQuery(..), ChildQueryBox)
import Halogen.Query.ChildQuery as CQ
import Halogen.Subscription (Subscription, subscribe)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Test.Spec.Halogen.Driver (AugmentedOutput(..), AugmentedQuery(..), TestHalogenM)
import Test.Spec.Halogen.Predicate (IncrementalPredicate, IsSatisfied(..), equals, finaliseIncremental, isSatisfied, runIncrementalFromEmitter, runIncrementalFromFoldable)
import Type.Proxy (Proxy)

shouldInduce :: forall state query action slots input output a
  .  Eq state => Eq action => Eq output
  => Show state => Show action => Show output
  => TestHalogenM state query action slots input output a
  -> IncrementalPredicate (AugmentedOutput state query action slots input output) 
  -> TestHalogenM state query action slots input output a
shouldInduce testHalogenM pred = do
  timeout <- asks (_.settings.timeout)
  messages <- asks (_.io.messages)

  _ <- liftEffect $ subscribe messages (\a -> log (show a))

  runIncrementalFromEmitter messages pred \ref -> do
    res <- testHalogenM
    liftAff (delay timeout)

    isSat <- liftEffect $ isSatisfied <$> Ref.read ref
    case isSat of
      Satisfied true -> pure res
      _ -> throwError (error (show isSat))


-- Operations
runAugmentedQuery :: forall state query action slots input output a
  .  AugmentedQuery state query action slots input output a
  -> TestHalogenM state query action slots input output (Maybe a)
runAugmentedQuery augmentedQuery = do
  (query :: forall x. AugmentedQuery state query action slots input output x -> Aff (Maybe x)) <- asks (_.io.query)
  liftAff $ query augmentedQuery

trigger :: forall state query action slots input output
  .  action 
  -> TestHalogenM state query action slots input output Unit
trigger action = void $ runAugmentedQuery (Trigger action)

componentTell :: forall state query action slots input output
  .  Tell query 
  -> TestHalogenM state query action slots input output Unit
componentTell tell = void $ runAugmentedQuery (ComponentTell tell)

componentRequest :: forall state query action slots input output a
  .  Request query a
  -> TestHalogenM state query action slots input output (Maybe a)
componentRequest req = runAugmentedQuery (ComponentRequest req)

childQueryBox
  :: forall label slots q o slot a _1
   . Row.Cons label (Slot q o slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> q a
  -> ChildQueryBox slots (Maybe a)
childQueryBox label p q = CQ.mkChildQueryBox $
  CQ.ChildQuery (\k storage -> maybe (pure Nothing) k (Slot.lookup label p storage)) q identity

childTell
  :: forall state query action slots input output label q o slot _1
   . Row.Cons label (Slot q o slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Tell q
  -> TestHalogenM state query action slots input output Unit
childTell label p tell =
  void $ runAugmentedQuery (ChildTell childQueryBox)

  where
    childQueryBox :: Tell (ChildQueryBox slots)
    childQueryBox _ = CQ.mkChildQueryBox childQuery

    childQuery :: ChildQuery slots q o Unit Maybe Unit
    childQuery =
      CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label p) (mkTell tell) (\_ -> unit)

childRequest
  :: forall state query action slots input output label q o slot a _1
   . Row.Cons label (Slot q o slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Request q a
  -> TestHalogenM state query action slots input output (Maybe a)
childRequest label p req = map join $
    runAugmentedQuery (ChildRequest childQueryBox)

  where
    childQueryBox :: Request (ChildQueryBox slots) (Maybe a)
    childQueryBox _ = CQ.mkChildQueryBox childQuery

    childQuery :: ChildQuery slots q o (Maybe a) Maybe a
    childQuery =
      CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label p) (mkRequest req) identity


-- Predicates
triggered :: forall state query action slots input output
  .  action 
  -> IncrementalPredicate (AugmentedOutput state query action slots input output)
triggered a = equals (Triggered a)

modified :: forall state query action slots input output
  .  state 
  -> IncrementalPredicate (AugmentedOutput state query action slots input output)
modified a = equals (Modified a)

raised :: forall state query action slots input output
  .  output 
  -> IncrementalPredicate (AugmentedOutput state query action slots input output)
raised a = equals (Raised a)