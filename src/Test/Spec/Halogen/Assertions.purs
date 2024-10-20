module Test.Spec.Halogen.Assertions where

import Prelude hiding (join)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Fork.Class (bracket, fork, join, kill, never, suspend)
import Control.Monad.Reader (asks)
import Data.Array (cons)
import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Fiber, delay, error, joinFiber, killFiber, launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Subscription)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith)
import Test.Halogen.Driver (TestComponentOutput(..))
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Halogen.Monad (TestHalogenM, componentState)

subscribeToOutput :: forall state query action output slots r. Eq action => Eq output
  => TestHalogenM state query action output slots Unit 
  -> (TestComponentOutput action output -> Maybe r)
  -> TestHalogenM state query action output slots (Maybe r)
subscribeToOutput testHalogenM pred = do
  ref <- liftEffect $ Ref.new Nothing
  fiber <- never 
  timeout <- asks (_.settings.timeout)
  _ <- bracket (sub ref fiber) (\_ -> unsub) \_ -> do
    testHalogenFiber <- fork testHalogenM


    {-
      testHalogen finiish
        - wait for delay <|> join fiber
      delay finishes 
        - cancel testHalogen, it should have finished already

      join fiber finshishes:
        return result
    -}
    try (liftAff (delay timeout) <|> join fiber <|> testHalogen)
  liftEffect (Ref.read ref)

  where
    sub ref fiber = do
      messages <- asks (_.io.messages)
      liftEffect $ HS.subscribe messages \o -> do
        for_ (pred o) \r -> do
          Ref.write (Just r) ref
          launchAff_ $ kill (error "output matching predicate found") fiber

    unsub id = liftEffect $ HS.unsubscribe id



--subscribeToOutputs :: forall state query action output slots r. Eq action => Eq output
--  => TestHalogenM state query action output slots Unit 
--  -> (TestComponentOutput action output -> Maybe r)
--  -> TestHalogenM state query action output slots (List r)
--subscribeToOutputs testHalogenM pred = do
--  ref <- liftEffect $ Ref.new []
--  timeout <- asks (_.settings.timeout)
--  bracket (subscribeToOutput ref) (\_ -> unsubscribeToOutput) \_ -> do
--    _ <- fork testHalogenM
--    runDelay timeout
--  liftEffect Ref.read ref
--  
--  where
--    runDelay timeout = do
--      liftAff (delay timeout)
--      log "failed to raise output"
--      throwError (error "failed to raise output")
--    
--    subscribeToOutput ref = do
--      messages <- asks (_.io.messages)
--      liftEffect $ HS.subscribe messages \o -> do
--        log "got message!"
--        for_ (pred o) \r -> do
--          log "output matching predicate found"
--          Ref.modify_ (cons r) ref
--
--    unsubscribeToOutput id = liftEffect $ HS.unsubscribe id

outputPredicate :: forall o a. Eq o => (o -> Boolean) -> (TestComponentOutput a o -> Maybe o)
outputPredicate pred = case _ of
  ComponentOutput o -> guard (pred o) $> o
  _ ->  Nothing

actionPredicate :: forall o a. Eq a => (a -> Boolean) -> (TestComponentOutput a o -> Maybe a)
actionPredicate pred = case _ of
  TriggeredAction a -> guard (pred a) $> a
  _ ->  Nothing

shouldRaise :: forall state query action output slots. Eq action => Eq output => Show output
  => TestHalogenM state query action output slots Unit 
  -> output
  -> TestHalogenM state query action output slots Unit
shouldRaise testHalogenM output = do
  raised <- subscribeToOutput testHalogenM (outputPredicate (eq output))
  unless (isJust raised) do
    throwError (error ("didn't raise " <> show output))

shouldRaiseAnything :: forall state query action output slots. Eq output => Eq action
  => TestHalogenM state query action output slots Unit 
  -> TestHalogenM state query action output slots Unit
shouldRaiseAnything testHalogenM = do
  raised <- subscribeToOutput testHalogenM (outputPredicate (\_ -> true))
  unless (isJust raised) do
    throwError (error ("didn't raise anything"))

{-
  trigger action `shouldTrigger` action
-}
shouldTrigger :: forall state query action output slots. Eq output => Eq action => Show action
  => TestHalogenM state query action output slots Unit
  -> action
  -> TestHalogenM state query action output slots Unit
shouldTrigger testHalogenM action = do
  output <- subscribeToOutput testHalogenM (actionPredicate (eq action))
  unless (isJust output) do
    throwError (error ("didn't trigger " <> show action))


{-
  ma `shouldTrigger` action => ma `shouldTriggerAnything`
-}
shouldTriggerAnything :: forall state query action output slots. Eq output => Eq action => Show action
  => TestHalogenM state query action output slots Unit 
  -> TestHalogenM state query action output slots Unit
shouldTriggerAnything testHalogenM = do
  output <- subscribeToOutput testHalogenM (actionPredicate (\_ -> true))
  unless (isJust output) do
    throwError (error ("didn't trigger anything"))

shouldHaveState :: forall state query action output slots. Eq state => Show state
  => state
  -> TestHalogenM state query action output slots Unit
shouldHaveState expected = do
  received <- componentState
  received `shouldEqual` expected

shouldHaveStateSatisfying :: forall state query action output slots. Eq state => Show state
  => (state -> Boolean) 
  -> TestHalogenM state query action output slots Unit
shouldHaveStateSatisfying pred = do
  received <- componentState
  received `shouldSatisfy` pred