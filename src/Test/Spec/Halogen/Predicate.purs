module Test.Spec.Halogen.Predicate where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extend, extract, (=<=))
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree as Cofree
import Control.Comonad.Env (EnvT(..))
import Control.Extend (extend)
import Control.Monad.Free (hoistFree)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, any, fold, foldM, foldMap, foldr)
import Data.Function (on)
import Data.Functor.Compose (Compose)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Functor.Mu (Mu(..), unroll)
import Data.Functor.Product (Product)
import Data.HeytingAlgebra (implies, tt)
import Data.Lens (Traversal')
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Maybe.First (First)
import Data.Monoid (power)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.Traversable (mapAccumR)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), bracket, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.HTML (p)
import Halogen.Subscription (Emitter, subscribe, unsubscribe)
import Matryoshka (cata)
import Node.FS.Constants (x_OK)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Boolean (True)
import Signal (Signal)
import Unsafe.Coerce (unsafeCoerce)

{-
  Observations are predicates that produce aritifacts
  The artifacts are used to sequence observations

-}
 
data PredicateF a b
  = Not b
  | Or b b
  | And b b
  | Sequence b b
  | Equals a
  | Satisfies (a -> IsSatisfied)
  | None
derive instance Functor (PredicateF a) 
derive instance Foldable (PredicateF a) 


data IsSatisfied = Unsatisfied | Satisfied Boolean
derive instance Eq IsSatisfied
instance HeytingAlgebra IsSatisfied where
  ff = Satisfied false
  tt = Satisfied true
  not = case _ of
    Unsatisfied -> Unsatisfied
    Satisfied b -> Satisfied (not b)
  disj a b = case a, b of
    Satisfied true, _ -> Satisfied true
    _, Satisfied true -> Satisfied true
    Satisfied false, Satisfied false -> Satisfied false
    _, _ -> Unsatisfied
  conj a b = case a, b of
    Satisfied false, _ -> Satisfied false
    _, Satisfied false -> Satisfied false
    Satisfied true, Satisfied true -> Satisfied true
    _, _ -> Unsatisfied
  implies a b = case a, b of
    Satisfied false, _ -> Satisfied true
    _, Satisfied true -> Satisfied true
    Satisfied true, Satisfied false -> Satisfied false
    _, _ -> Unsatisfied
instance Show IsSatisfied where
  show = case _ of
    Unsatisfied -> "Unsatisfied"
    Satisfied b -> "Satisfied " <> show b
  
newtype IncrementalPredicate a = Incremental (Cofree (PredicateF a) IsSatisfied)
derive instance Newtype (IncrementalPredicate a) _
instance Show a => Show (IncrementalPredicate a) where
  show (Incremental cofree) = intercalate "\n" $
    cata printPred =<= cata printNode $ cofree
    where
      -- cata printPred :: Cofree (PredicateF a) String -> Array String
      printPred :: EnvT String (PredicateF a) (Array String) -> Array String
      printPred (EnvT (Tuple str pred)) = [ str ] <> map (append "  ") (fold pred)

      -- cata printNode :: Cofree (PredicateF a) IsSatisfied -> String
      printNode :: forall x. EnvT IsSatisfied (PredicateF a) x -> String
      printNode (EnvT (Tuple isSat pred)) = "(" <> show isSat <> ") " <> case pred of
        Not x -> "Not:"
        Or x y -> "Or:"
        And x y -> "And:"
        Sequence x y -> "Seq:"
        Equals expecting -> "Equals " <> show expecting
        Satisfies p -> "Satisfied"
        None -> "None"
instance HeytingAlgebra (IncrementalPredicate a) where
  ff = Incremental (Satisfied false :< None)
  tt = Incremental (Satisfied true :< None)
  not = not_
  disj = or_
  conj = and_
  implies x y = not_ x `and_` y

incrementalPredicate :: Cofree (PredicateF a) IsSatisfied -> IncrementalPredicate a
incrementalPredicate cofree = Incremental ()
  where
    f :: Cofree (PredicateF a) IsSatisfied -> IsSatisfied
    f cofree = case Cofree.tail cofree of 
      Not x -> not (Cofree.head x)
      Or  x y -> disj (Cofree.head x) (Cofree.head y)
      And x y -> conj (Cofree.head x) (Cofree.head y)
      Sequence x y -> conj (Cofree.head x) (Cofree.head y)
      _ -> Cofree.head cofree
    


toArray :: forall a b. PredicateF a b -> Array b
toArray = case _ of
  Not x -> [ x ]
  Or x y -> [ x, y ]
  And x y -> [ x, y ]
  Sequence x y -> [ x, y ]
  _ -> [] 

isSatisfied :: forall a. IncrementalPredicate a -> IsSatisfied
isSatisfied (Incremental cofree) = Cofree.head cofree

runIncremental :: forall a. Eq a => a -> IncrementalPredicate a -> IncrementalPredicate a
runIncremental a = unwrap >>> go >>> wrap
  where
    go :: Cofree (PredicateF a) IsSatisfied -> Cofree (PredicateF a) IsSatisfied
    go cofree = if Cofree.head cofree == Unsatisfied then (head :< tail) else cofree
      where 
        head :: IsSatisfied
        head = case tail of 
          Not x -> not (Cofree.head x)
          Or  x y -> disj (Cofree.head x) (Cofree.head y)
          And x y -> conj (Cofree.head x) (Cofree.head y)
          Sequence x y -> conj (Cofree.head x) (Cofree.head y)
          Equals expected -> if a == expected then Satisfied true else Unsatisfied
          Satisfies p -> p a 
          None -> Unsatisfied

        tail :: PredicateF a (Cofree (PredicateF a) IsSatisfied)
        tail = case Cofree.tail cofree of 
          Not x -> Not (go x)
          Or  x y -> Or  (go x) (go y)
          And x y -> And (go x) (go y)
          Sequence x y -> case Cofree.head x of
            Unsatisfied -> Sequence (go x) y
            Satisfied false -> Sequence x y
            Satisfied true -> Sequence x (go y)
          Equals expected -> Equals expected 
          Satisfies p -> Satisfies p
          None -> None

finaliseIncremental :: forall a. IncrementalPredicate a -> IncrementalPredicate a
finaliseIncremental (Incremental x) = incrementalPredicate (extend f x)
  where
    f :: Cofree (PredicateF a) IsSatisfied -> IsSatisfied
    f cofree = case Cofree.tail cofree of
      Not x -> if Cofree.head x == Unsatisfied then Satisfied true else Cofree.head x
      x -> Cofree.head x

repeatExactly :: forall a. Int -> IncrementalPredicate a -> IncrementalPredicate a
repeatExactly 0 incremental = not incremental
repeatExactly n incremental = incremental `then_` repeatExactly (n-1) incremental
-- can i do repeatExactly = repeatAtLeast && repeatAtMost ?

repeatAtLeast :: forall a. Int -> IncrementalPredicate a -> IncrementalPredicate a
repeatAtLeast 0 _ = tt
repeatAtLeast n incremental = incremental `then_` repeatAtLeast (n-1) incremental

repeatAtMost :: forall a. Int -> IncrementalPredicate a -> IncrementalPredicate a
repeatAtMost 0 incremental = not incremental
repeatAtMost n incremental = incremental `then_` repeatAtLeast (n-1) incremental

-- should be short circuiting but is not!
runIncrementalFromEmitter :: forall a. Eq a => Emitter a -> IncrementalPredicate a -> Aff (Ref (IncrementalPredicate a))
runIncrementalFromEmitter emitter pred = do
  ref <- liftEffect (Ref.new pred)
  let sub = liftEffect (subscribe emitter (\a -> Ref.modify_ (runIncremental a) ref))
  let unsub = liftEffect <<< unsubscribe
  bracket sub unsub \_ -> do
    pure ref

runIncrementalFromFoldable :: forall f a. Foldable f => Eq a => f a -> IncrementalPredicate a -> IncrementalPredicate a
runIncrementalFromFoldable values pred = either identity identity $ foldM (flip f) pred values
  where
    f :: a -> IncrementalPredicate a -> Either (IncrementalPredicate a) (IncrementalPredicate a) 
    f a i = do
      let i' = runIncremental a i
      case isSatisfied i' of
        Unsatisfied -> Right i'
        Satisfied _ -> Left i'

equals :: forall a. a -> IncrementalPredicate a
equals a = Incremental (Unsatisfied :< Equals a)

satisfies :: forall a. (a -> Boolean) -> IncrementalPredicate a
satisfies p = Incremental $ Unsatisfied :< Satisfies (\a -> if p a then Satisfied true else Unsatisfied)

not_ :: forall a. IncrementalPredicate a -> IncrementalPredicate a
not_ (Incremental x) =
  Incremental ((not (Cofree.head x)) :< Not x)

or_ :: forall a. IncrementalPredicate a -> IncrementalPredicate a -> IncrementalPredicate a
or_ (Incremental x) (Incremental y) =
  Incremental ((Cofree.head x || Cofree.head y) :< Or x y)

and_ :: forall a. IncrementalPredicate a -> IncrementalPredicate a -> IncrementalPredicate a
and_ (Incremental x) (Incremental y) = 
  Incremental ((Cofree.head x && Cofree.head y) :< And x y)

then_ :: forall a. IncrementalPredicate a -> IncrementalPredicate a -> IncrementalPredicate a
then_ (Incremental x) (Incremental y) =
  Incremental ((Cofree.head x && Cofree.head y) :< Sequence x y)