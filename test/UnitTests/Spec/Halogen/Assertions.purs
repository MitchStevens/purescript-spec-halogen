module UnitTests.Spec.Halogen.Assertions where

import Prelude

import Component.Counter (Action(..), Query(..))
import Component.Counter as Counter
import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Spec.Halogen.Assertions (shouldInduce, trigger)
import Test.Spec.Halogen.Driver (AugmentedOutput(..), getComponentState, mkTestComponent, withComponent)

spec :: Spec Unit
spec = tests 


tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  withComponent Counter.componentSpec unit do
    describe "Counter component" do
      describe "State" do
        it "should have state" $ runReaderT do
          s <- getComponentState
          s.count `shouldEqual` 0

        describe "it should not retain state between tests" do
          it "test1" $ runReaderT do
            trigger (SetValue "1")
            s <- getComponentState
            s.count `shouldEqual` 1
          it "test2" $ runReaderT do
            s <- getComponentState
            s.count `shouldEqual` 0

        --it "should" $ runReaderT do
        --  trigger (SetValue "1") `shouldInduce` modified { count: 1, isPondering: false }
{-
      describe "Outputs" do
        it "should detect outputs" $ runReaderT do
          tell Increment 
          tell Increment 
          trigger Double
          tell Increment 
          trigger Double `shouldRaise` Equals10
          shouldHaveStateSatisfying \s -> s.count == 10

        it "should detect the absence of outputs" $ runReaderT do
          tell Increment
          tell Increment
          trigger Quadruple
          expectError $
            tell Increment `shouldRaise` Equals10
          shouldHaveStateSatisfying \s -> s.count == 9
          tell Increment `shouldRaise` Equals10
          shouldHaveStateSatisfying \s -> s.count == 10

        it "should fail when the Output takes too long to be raised" $ runReaderT do
          expectError $
            tell (Counter.Ponder (Milliseconds 300.0)) `shouldRaise` FinishedPondering

        it "should pass when timeout duration is increased" $ runReaderT do 
          local (_ { settings { timeout = Milliseconds 400.0 }}) do
            tell (Counter.Ponder (Milliseconds 300.0)) `shouldRaise` FinishedPondering
      
      -}