module Test.Component.Summation where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_, sum)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.=))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML, ComponentSpec, HalogenM, Slot, requestAll)
import Halogen as H
import Halogen.HTML as HH
import Test.Component.Counter as Counter
import Type.Proxy (Proxy(..))

type Input = 
  { numCounters :: Int
  }

type State = 
  { numCounters :: Int
  , total :: Int
  }

data Query a

data Action
  = CounterOutput Counter.Output
derive instance Generic Action _
derive instance Eq Action
instance Show Action where
  show = genericShow

type Slots = ( counterSlot :: Slot Counter.Query Counter.Output Int )

data Output
  = Total Int
derive instance Generic Output _
derive instance Eq Output
instance Show Output where
  show = genericShow

_total :: Lens' State Int
_total = prop (Proxy :: Proxy "total")

component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent componentSpec

componentSpec :: forall m. MonadAff m => ComponentSpec State Query Action Slots Input Output m
componentSpec = 
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval 
        { handleAction = \action -> handleAction action
        , handleQuery = \query -> handleQuery query
        })
    }
  where
    handleAction :: Action -> HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      CounterOutput output -> case output of
        Counter.Equals10 -> pure unit
        Counter.FinishedPondering -> pure unit
        Counter.CountChanged -> do
          counts <- requestAll (Proxy :: Proxy "counterSlot") Counter.GetCount
          _total .= sum counts


    handleQuery :: forall a. Query a -> HalogenM State Action Slots Output m (Maybe a)
    handleQuery = case _ of
      _ -> pure Nothing

    initialState :: Input -> State
    initialState { numCounters } =  { numCounters, total: 0 }

    render :: State -> ComponentHTML Action Slots m
    render state = HH.div_ $
      flip map (0..state.numCounters) \i ->
        HH.slot (Proxy :: Proxy "counterSlot") i Counter.component unit CounterOutput


