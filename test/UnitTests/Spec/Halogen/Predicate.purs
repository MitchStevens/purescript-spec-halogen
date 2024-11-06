module UnitTests.Spec.Halogen.Predicate where

import Prelude
import Test.Spec.Halogen.Predicate

import Control.Extend (extend)
import Data.Foldable (class Foldable, foldr, for_, or)
import Data.HeytingAlgebra (ff, tt)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (expectError, shouldEqual)

--runIncremental :: forall a. Eq a => IncrementalPredicate a -> Array a -> Boolean
--runIncremental incremental array =
--  isSatisfied $ foldr (\a -> extend (runPredicate a)) incremental array



spec :: Spec Unit
spec = describe "Test.Spec.Halogen.Predicate" do
  let incremental = 
        { equals: equals 1
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

  describe "IncrementalPredicate" do
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
      pending ""
  
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