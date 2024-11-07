module Test.Spec.Halogen.Assertions where

import Prelude hiding (join)

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Except (ExceptT, lift, throwError)
import Control.Monad.Fork.Class (bracket, fork, join, kill, never, suspend)
import Control.Monad.Reader (asks)
import Data.Array (cons)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldM, for_)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff, Fiber, delay, error, joinFiber, killFiber, launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Subscription)
import Halogen.Subscription as HS
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec.Assertions (fail)
import Test.Spec.Halogen.Driver (AugmentedOutput(..), AugmentedQuery(..), TestHalogenM)
import Test.Spec.Halogen.Predicate (IncrementalPredicate, IsSatisfied(..), equals, finaliseIncremental, isSatisfied, runIncremental, runIncrementalFromEmitter, runIncrementalFromFoldable)

--shouldSatisfy :: forall f a. Foldable f => Eq a => Show a => f a -> IncrementalPredicate a -> Aff Unit
--shouldSatisfy values incremental = do
--  let incremental' = runAll values incremental
--  case isSatisfied incremental' of
--    Unsatisfied -> fail (show incremental')
--    Satisfied true -> pure unit
--    Satisfied false -> fail (show incremental')
  





--subscribeToOutput :: forall state query action output slots r. Eq action => Eq output
--  => TestHalogenM state query action output slots Unit 
--  -> (TestComponentOutput action output -> Maybe r)
--  -> TestHalogenM state query action output slots (Maybe r)
--subscribeToOutput testHalogenM pred = do
--  ref <- liftEffect $ Ref.new Nothing
--  fiber <- never 
--  timeout <- asks (_.settings.timeout)
--  _ <- bracket (sub ref fiber) (\_ -> unsub) \_ -> do
--    testHalogenFiber <- fork testHalogenM
--
--
--    {-
--      testHalogen finiish
--        - wait for delay <|> join fiber
--      delay finishes 
--        - cancel testHalogen, it should have finished already
--
--      join fiber finshishes:
--        return result
--    -}
--    try (liftAff (delay timeout) <|> join fiber <|> testHalogen)
--  liftEffect (Ref.read ref)
--
--  where
--    sub ref fiber = do
--      messages <- asks (_.io.messages)
--      liftEffect $ HS.subscribe messages \o -> do
--        for_ (pred o) \r -> do
--          Ref.write (Just r) ref
--          launchAff_ $ kill (error "output matching predicate found") fiber
--
--    unsub id = liftEffect $ HS.unsubscribe id
--
--
--
----subscribeToOutputs :: forall state query action output slots r. Eq action => Eq output
----  => TestHalogenM state query action output slots Unit 
----  -> (TestComponentOutput action output -> Maybe r)
----  -> TestHalogenM state query action output slots (List r)
----subscribeToOutputs testHalogenM pred = do
----  ref <- liftEffect $ Ref.new []
----  timeout <- asks (_.settings.timeout)
----  bracket (subscribeToOutput ref) (\_ -> unsubscribeToOutput) \_ -> do
----    _ <- fork testHalogenM
----    runDelay timeout
----  liftEffect Ref.read ref
----  
----  where
----    runDelay timeout = do
----      liftAff (delay timeout)
----      log "failed to raise output"
----      throwError (error "failed to raise output")
----    
----    subscribeToOutput ref = do
----      messages <- asks (_.io.messages)
----      liftEffect $ HS.subscribe messages \o -> do
----        log "got message!"
----        for_ (pred o) \r -> do
----          log "output matching predicate found"
----          Ref.modify_ (cons r) ref
----
----    unsubscribeToOutput id = liftEffect $ HS.unsubscribe id
--
--outputPredicate :: forall o a. Eq o => (o -> Boolean) -> (TestComponentOutput a o -> Maybe o)
--outputPredicate pred = case _ of
--  ComponentOutput o -> guard (pred o) $> o
--  _ ->  Nothing
--
--actionPredicate :: forall o a. Eq a => (a -> Boolean) -> (TestComponentOutput a o -> Maybe a)
--actionPredicate pred = case _ of
--  TriggeredAction a -> guard (pred a) $> a
--  _ ->  Nothing
--
--shouldRaise :: forall state query action output slots. Eq action => Eq output => Show output
--  => TestHalogenM state query action output slots Unit 
--  -> output
--  -> TestHalogenM state query action output slots Unit
--shouldRaise testHalogenM output = do
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


shouldInduce :: forall state query action slots input output a
  .  Eq state => Eq action => Eq input => Eq output =>
  TestHalogenM state query action slots input output a
  -> IncrementalPredicate (AugmentedOutput state query action slots input output) 
  -> TestHalogenM state query action slots input output a
shouldInduce testHalogenM pred = do
  timeout <- asks (_.settings.timeout)
  messages <- asks (_.io.messages)
  ref <- liftAff (runIncrementalFromEmitter messages pred)
  res <- testHalogenM
  liftAff (delay timeout)
  isSat <- liftEffect $ isSatisfied <$> Ref.read ref
  case isSat of
    Satisfied true -> pure res
    _ -> throwError (error (show isSat))

triggered :: forall state query action slots input output
  .  action 
  -> IncrementalPredicate (AugmentedOutput state query action slots input output)
triggered a = equals (Triggered a)

trigger :: forall state query action slots input output
  .  action 
  -> TestHalogenM state query action slots input output Unit
trigger action = do
  query <- asks (_.io.query)
  _ <- liftAff $ query (Trigger action)
  pure unit


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
--shouldHaveState :: forall state query action output slots. Eq state => Show state
--  => state
--  -> TestHalogenM state query action output slots Unit
--shouldHaveState expected = do
--  received <- componentState
--  received `shouldEqual` expected
--
--shouldHaveStateSatisfying :: forall state query action output slots. Eq state => Show state
--  => (state -> Boolean) 
--  -> TestHalogenM state query action output slots Unit
--shouldHaveStateSatisfying pred = do
--  received <- componentState
--  received `shouldSatisfy` pred


{-
  Used for testing `IncrementalPredicate`s
-}
shouldBeSatisfiedBy :: forall f a. Foldable f => Eq a => Show a
  => IncrementalPredicate a
  -> f a
  -> Aff Unit
shouldBeSatisfiedBy pred values = do
  let final = finaliseIncremental (runIncrementalFromFoldable values pred)
  unless (isSatisfied final == Satisfied true) do
    throwError (error (show final))




