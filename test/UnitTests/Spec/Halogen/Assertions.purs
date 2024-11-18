module UnitTests.Spec.Halogen.Assertions where

import Prelude

import Component.Counter (Action(..), Output(..), Query(..))
import Component.Counter as Counter
import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..))
import Test.Spec (Spec, SpecT, describe, describeOnly, it, pending)
import Test.Spec.Assertions (expectError, shouldEqual, shouldReturn)
import Test.Spec.Halogen.Assertions (componentRequest, componentTell, modified, raised, shouldInduce, trigger, triggered)
import Test.Spec.Predicate (repeatExactly)

spec :: Spec Unit
spec = tests 


tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  pure unit

{-
  describe "Test.Spec.Halogen.Assertion" do
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
      describe "trigger" do
        it "" $ runReaderT do
          trigger (SetValue "1") `shouldInduce` triggered (SetValue "1")
        it "" $ runReaderT do
          trigger (SetValue "1") `shouldInduce` raised CountChanged
        it "Quadruple should" $ runReaderT do
          trigger Quadruple `shouldInduce` (triggered Double)

      describe "componentTell" do
        it "" $ runReaderT do
          expectError $
            componentTell DoNothing `shouldInduce` raised CountChanged
        it "" $ runReaderT do
          componentTell (Ponder (Milliseconds 200.0)) `shouldInduce` raised FinishedPondering
            `setTimeout` (Milliseconds 210.0)
        it "should detect outputs" $ runReaderT do
          componentTell Increment 
          componentTell Increment 
          trigger Double
          componentTell Increment 
          trigger Double `shouldInduce` raised Equals10
          --trigger Double 
          --s <- getComponentState
          --s.count `shouldEqual` 10
      describe "componentRequest" do
        it "" $ runReaderT do
          count <- componentRequest GetCount
          count `shouldEqual` Just 0
        it "" $ runReaderT do
          trigger (SetValue "22")
          count <- componentRequest GetCount
          count `shouldEqual` Just 22



{-

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