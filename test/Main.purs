module Test.Main
  ( main
  )
  where

import Prelude

import Component.Counter (Action(..), Output(..), Query(..))
import Component.Counter as Counter
import Component.Summation as Summation
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (local, runReaderT)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), catchError, launchAff_, runAff_)
import Effect.Class.Console (log, logShow)
import Halogen (liftEffect)
import Halogen as H
import Halogen as Test
import Test.Spec (SpecT, before, describe, it, itOnly, parallel, pending)
import Test.Spec.Assertions (expectError)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess, runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)
import Type.Proxy (Proxy(..))
import UnitTests.Spec.Halogen.Assertions as Assertions
import UnitTests.Spec.Halogen.Predicate as Predicate

main :: Effect Unit
main = launchAff_ do
--  specs <- discover "Test.*"

  liftEffect $ runSpecAndExitProcess [ consoleReporter ] do
    Assertions.spec
    Predicate.spec

{-
main = runSpecAndExitProcess [ consoleReporter ] tests

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = pure unit
{-
tests = do
  withComponent Counter.componentSpec unit do
    describe "Counter component" do
      describe "Action" do
        it "should trigger actions" $ runReaderT do
          tell Increment

      describe "State" do
        it "should have state" $ runReaderT do
          shouldHaveState (Counter.initialState unit)
        
        it "can check if state satisfies predicates" $ runReaderT do
          shouldHaveStateSatisfying (\s -> s.count == 0)
          expectError $
            shouldHaveStateSatisfying (\s -> s.count * s.count > 0)

        describe "it should not retain state between tests" do
          it "test1" $ runReaderT do
            tell Increment
            shouldHaveStateSatisfying (\s -> s.count == 1)
          it "test2" $ runReaderT do
            shouldHaveStateSatisfying (\s -> s.count == 0)

      describe "Outputs" do
        it "should detect outputs" $ runReaderT do
          tell Increment 
          tell Increment 
          trigger Double
          tell Increment 
          trigger Double `shouldRaise` Equals10
          shouldHaveStateSatisfying \s -> s.count == 10

--        it "should detect the absence of outputs" $ runReaderT do
--          tell Increment
--          tell Increment
--          trigger Quadruple
--          expectError $
--            tell Increment `shouldRaise` Equals10
--          shouldHaveStateSatisfying \s -> s.count == 9
--          tell Increment `shouldRaise` Equals10
--          shouldHaveStateSatisfying \s -> s.count == 10
--
--        it "should fail when the Output takes too long to be raised" $ runReaderT do
--          expectError $
--            tell (Counter.Ponder (Milliseconds 300.0)) `shouldRaise` FinishedPondering
--
--        it "should pass when timeout duration is increased" $ runReaderT do 
--          local (_ { settings { timeout = Milliseconds 400.0 }}) do
--            tell (Counter.Ponder (Milliseconds 300.0)) `shouldRaise` FinishedPondering
--      
--  withComponent (mkTestComponent Summation.componentSpec) { numCounters: 5 } do
--    describe "Slots" do
--      it "can query children" $ runReaderT do
--        shouldHaveStateSatisfying \s -> s.total == 0
--        childTell (Proxy :: _ "counterSlot") 0 Increment `shouldTrigger` Summation.CounterOutput Counter.CountChanged
--        shouldHaveStateSatisfying \s -> s.total == 1