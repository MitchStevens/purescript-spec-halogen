module Test.Spec.Halogen.Assertions where

import Prelude hiding (join)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (bracket, fork, join, kill, never, suspend)
import Control.Monad.Reader (asks)
import Data.Array (cons)
import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect.Aff (Aff, Fiber, delay, error, joinFiber, killFiber, launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Subscription)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec.Halogen.Predicate (IncrementalPredicate)

--
--accum :: forall a. Signal a -> Signal (Array a)
--accum = Signal.foldp (flip A.snoc) []
--
--accumMaybe :: forall a. Signal (Maybe a) -> Signal (Array a)
--accumMaybe = Signal.foldp (maybe (identity) (flip A.snoc)) []


--shouldRaisePredicate :: forall state query action slots input output. Eq output => Show output
--  => TestHalogenM state query action output slots Unit 
--  -> (output -> Boolean)
--  -> TestHalogenM state query action output slots Unit
--shouldRaisePredicate testHalogenM pred = do
--  outputs <- foldp (maybe identity (flip A.snoc) [] <$> asks _.outputChannel 
--  testHalogenM
--
--
--
--
--shouldRaise :: forall state query action output slots. Eq action => Eq output => Show output
--  => TestHalogenM state query action output slots Unit 
--  -> output
--  -> TestHalogenM state query action output slots Unit
--shouldRaise testHalogenM output = do
--  foldp () [] (asks _.outputChannel)
--
--
--  raised <- subscribeToOutput testHalogenM (outputPredicate (eq output))
--  unless (isJust raised) do
--    throwError (error ("didn't raise " <> show output))
--
--shouldRaiseAnything :: forall state query action output slots. Eq output => Eq action
--  => TestHalogenM state query action output slots Unit 
--  -> TestHalogenM state query action output slots Unit
--shouldRaiseAnything testHalogenM = do
--  raised <- subscribeToOutput testHalogenM (outputPredicate (\_ -> true))
--  unless (isJust raised) do
--    throwError (error ("didn't raise anything"))
--
--{-
--  trigger action `shouldTrigger` action
---}
--shouldTrigger :: forall state query action output slots. Eq output => Eq action => Show action
--  => TestHalogenM state query action output slots Unit
--  -> action
--  -> TestHalogenM state query action output slots Unit
--shouldTrigger testHalogenM action = do
--  output <- subscribeToOutput testHalogenM (actionPredicate (eq action))
--  unless (isJust output) do
--    throwError (error ("didn't trigger " <> show action))
--
--
--{-
--  ma `shouldTrigger` action => ma `shouldTriggerAnything`
---}
--shouldTriggerAnything :: forall state query action output slots. Eq output => Eq action => Show action
--  => TestHalogenM state query action output slots Unit 
--  -> TestHalogenM state query action output slots Unit
--shouldTriggerAnything testHalogenM = do
--  output <- subscribeToOutput testHalogenM (actionPredicate (\_ -> true))
--  unless (isJust output) do
--    throwError (error ("didn't trigger anything"))
--
--shouldHaveState :: forall state query action slots input output. Eq state => Show state
--  => state
--  -> TestHalogenM state query action slots input output Unit
--shouldHaveState expected = do
--  received <- asks (_.stateSignal) >>= (Signal.get >>> liftEffect)
--  received `shouldEqual` expected
--
--shouldHaveStateSatisfying :: forall state query action slots input output. Eq state => Show state
--  => (state -> Boolean) 
--  -> TestHalogenM state query action slots input output Unit
--shouldHaveStateSatisfying pred = do
--  received <- asks (_.stateSignal) >>= (Signal.get >>> liftEffect)
--  received `shouldSatisfy` pred

shouldSatisfy :: forall f. Foldable f => f a -> IncrementalPredicate a -> Aff Unit
shouldSatisfy inputs predicate = 

--shouldFailToSatisfy :: forall f. Foldable f => f a -> IncrementalPredicate a -> Aff Unit