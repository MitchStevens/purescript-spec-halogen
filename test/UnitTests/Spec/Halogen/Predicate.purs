module UnitTests.Spec.Halogen.Predicate where

import Prelude
import Test.Spec.Halogen.Predicate

import Control.Comonad.Cofree ((:<))
import Control.Extend (extend)
import Data.Array (snoc)
import Data.Foldable (class Foldable, foldr, for_, or)
import Data.HeytingAlgebra (ff, tt)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, makeEmitter)
import Halogen.Subscription as HS
import Test.Spec (Spec, describe, it, itOnly, pending)
import Test.Spec.Assertions (expectError, shouldEqual)


spec :: Spec Unit
spec = describe "Test.Spec.Halogen.Predicate" do
  let incremental = 
        { equals: equals 1
        , not: not (equals 1)
        , or: equals 1 || equals 2
        , and: equals 1 && equals 2
        , then: equals 1 `then_` equals 2
        }

  it "isSatisfied" do
    isSatisfied ff `shouldEqual` Satisfied false
    isSatisfied tt `shouldEqual` Satisfied true
    isSatisfied (equals 0) `shouldEqual` Unsatisfied
    isSatisfied (ff `then_` tt) `shouldEqual` Satisfied false
    isSatisfied (tt `then_` tt) `shouldEqual` Satisfied true
  
  it "finaliseIncremental" do
    isSatisfied (finaliseIncremental (equals 1)) `shouldEqual` Unsatisfied

  --describe "repeatAtLeast" do
  --  let incremental = repeatAtLeast 2 (equals 0)
  --  it "should fail when number of observations < n" do
  --    expectError $ incremental `shouldBeSatisfiedBy` []
  --    expectError $ incremental `shouldBeSatisfiedBy` [0]
  --  it "should pass when number of observations >= n" do
  --      incremental `shouldBeSatisfiedBy` [0, 0]
  --      incremental `shouldBeSatisfiedBy` [0, 0, 0, 0]
  --      incremental `shouldBeSatisfiedBy` [1, 0, 1, 0]
  --describe "repeatAtMost" do
  --  let incremental = not (repeatAtLeast 2 (equals 0))
  --  it "should fail when number of observations > n" do
  --    incremental `shouldBeSatisfiedBy` []
      --incremental `shouldBeSatisfiedBy` [0]
      --incremental `shouldBeSatisfiedBy` [0, 0]
    --it "should pass when number of observations <= n" do
    --  expectError $ incremental `shouldBeSatisfiedBy` [0, 0, 0, 0]
    --  expectError $ incremental `shouldBeSatisfiedBy` [1, 0, 1, 0]
  
  --describe "HeytingAlgebra laws" do
  --  let bool = pure tt <|> pure ff
  --  it "associativity" do
  --    quickCheck do


  describe "incrementalPredicate" do
    it "should recompute isSat labels" do
      isSatisfied (incrementalPredicate (Satisfied true :< None)) `shouldEqual` Satisfied true
      isSatisfied (incrementalPredicate (Unsatisfied :< Not (Satisfied true :< None))) `shouldEqual` Satisfied false

  describe "runIncremental" do 
    it "equals" do
      isSatisfied incremental.equals `shouldEqual` Unsatisfied
      isSatisfied (runIncremental 0 incremental.equals) `shouldEqual` Unsatisfied
      isSatisfied (runIncremental 1 incremental.equals) `shouldEqual` Satisfied true

    it "or" do
      isSatisfied incremental.or `shouldEqual` Unsatisfied
      isSatisfied (runIncremental 1 incremental.or) `shouldEqual` Satisfied true
      isSatisfied (runIncremental 2 incremental.or) `shouldEqual` Satisfied true


    it "then_" do
      isSatisfied (runIncremental 2 $ runIncremental 1 incremental.then) `shouldEqual` Satisfied true

      --isSatisfied incremental.then `shouldEqual` Unsatisfied
      pure unit
  describe "runPredicateFromFoldable" do
    pending ""
  describe "runPredicateFromEmitter" do
    it "emitFoldable should faithfully emit values from a foldable" do
      emittedValues <- liftEffect (Ref.new [])
      let emitter = emitFoldable [ 1, 2, 3, 4, 5 ]
      _ <- liftEffect $ HS.subscribe emitter (\a -> Ref.modify_ (_ `snoc` a) emittedValues)
      delay (Milliseconds 100.0)
      values <- liftEffect $ Ref.read emittedValues
      values `shouldEqual` [ 1, 2, 3, 4, 5 ]
    it "should pass the following tests" do
      let
        emitterTest :: forall a. Eq a => Show a => Array a -> IncrementalPredicate a -> IsSatisfied -> Aff Unit
        emitterTest values p isSat = do
          runIncrementalFromEmitter (emitFoldable values) p \ref -> do
            incremental <- liftEffect (Ref.read ref)
            isSatisfied incremental `shouldEqual` isSat

      
      emitterTest [] incremental.not Unsatisfied
      emitterTest [1] incremental.not (Satisfied false)

      emitterTest [] incremental.or Unsatisfied
      emitterTest [1] incremental.or (Satisfied true)

      emitterTest []     incremental.and Unsatisfied
      emitterTest [1]    incremental.and Unsatisfied
      emitterTest [2]    incremental.and Unsatisfied
      emitterTest [1, 2] incremental.and (Satisfied true)

      emitterTest []     incremental.then Unsatisfied
      emitterTest [1]    incremental.then Unsatisfied
      emitterTest [2]    incremental.then Unsatisfied
      emitterTest [2, 1] incremental.then Unsatisfied
      emitterTest [1, 2] incremental.then (Satisfied true)

      
    



  describe "Advanced Predicates" do
    let even = or (map equals [0, 2, 4, 6, 8])
    let odd  = or (map equals [1, 3, 5, 7, 9])
    let alternatingEvenOdd = let evenThenOdd = even `then_` odd in evenThenOdd `then_` evenThenOdd
    it "passes for even numbers" do
      for_ [0, 2, 4, 6, 8] \n -> 
        isSatisfied (runIncremental n even) `shouldEqual` Satisfied true
    it "passes for odd numbers" do
      for_ [1, 3, 5, 7, 9] \n -> 
        isSatisfied (runIncremental n odd) `shouldEqual` Satisfied true
    it "passes for alternating sequences of numbers" do
      isSatisfied (runIncrementalFromFoldable [0, 1, 2, 3] alternatingEvenOdd) `shouldEqual` Satisfied true
      isSatisfied (runIncrementalFromFoldable [0, 1, 5, 7, 2, 8, 3] alternatingEvenOdd) `shouldEqual` Satisfied true
      isSatisfied (runIncrementalFromFoldable [0, 2, 1, 3] alternatingEvenOdd) `shouldEqual` Unsatisfied


--      it "or (equals 1) (equals 2)" do
--        let incremental = or (equals 1) (equals 2)
--
--        isSatisfied incremental `shouldEqual` false
--        isSatisfied (runIncremental 0 incremental) `shouldEqual` false
--        isSatisfied (runIncremental 1 incremental) `shouldEqual` true
--        isSatisfied (runIncremental 2 incremental) `shouldEqual` true
--

--        isSatisfied (foldr runIncremental incremental [ 0, 0 ]) `shouldEqual` false
--        isSatisfied (foldr runIncremental incremental [ 1, 0 ]) `shouldEqual` true
--      it "and (equals 1) (equals 2)" do
--        let incremental = and (equals 1) (equals 2)
--        isSatisfied incremental `shouldEqual` false
--        isSatisfied (runIncremental 0 incremental) `shouldEqual` false
--        isSatisfied (runIncremental 1 incremental) `shouldEqual` false
--        isSatisfied (runAll [ 0, 0 ] incremental) `shouldEqual` false
--        isSatisfied (runAll [ 0, 1 ] incremental) `shouldEqual` false
--        isSatisfied (runAll [ 1, 2 ] incremental) `shouldEqual` true
--        isSatisfied (runAll [ 2, 1 ] incremental) `shouldEqual` true

        --let fedTwo = runIncremental 2 incremental
        --fedTwo `shouldEqual` incremental
--        
--
--
--        --isSatisfied (foldr runIncremental incremental [1])
----
----
        --isSatisfied (runIncrementalFromFoldable [ 0, 0 ] incremental) `shouldEqual` Unsatisfied
        --isSatisfied (runIncrementalFromFoldable [ 0, 1 ] incremental) `shouldEqual` Unsatisfied
--        expectError $
--          [ 1 ] `shouldSatisfy` incremental
--
--        [ 1, 2 ] `shouldSatisfy` incremental
    --it ""

emitFoldable :: forall f a. Foldable f => f a -> Emitter a
emitFoldable values = makeEmitter \callback -> do
  for_ values \a -> do
    launchAff_ (delay (Milliseconds 10.0))
    callback a
  pure (pure unit) -- this emitter is un-unsubscribable

readAff :: forall a. Aff (Ref a) -> Aff a
readAff a = a >>= \ref -> liftEffect (Ref.read ref)