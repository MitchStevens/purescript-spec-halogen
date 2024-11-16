module Component.Counter where

import Prelude

import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Lens (Lens', use, (*=), (+=), (-=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (now)
import Halogen (ComponentSpec, HalogenM, ComponentHTML, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Input = Unit

type State = 
  { count :: Int
  , isPondering :: Boolean
  }

_count :: Lens' State Int
_count = prop (Proxy :: _ "count")

_isPondering :: Lens' State Boolean
_isPondering = prop (Proxy :: _ "isPondering")

data Query a
  = GetCount (Int -> a)
  | DoNothing a
  | Ponder Milliseconds a
  | Increment a
  | Decrement a

data Action = SetValue String | Double | Quadruple
derive instance Generic Action _
derive instance Eq Action
instance Show Action where
  show = genericShow

type Slots = ()

data Output
  = Equals10
  | FinishedPondering
  | CountChanged
derive instance Generic Output _
derive instance Eq Output
instance Show Output where
  show = genericShow



component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent componentSpec

componentSpec :: forall m. MonadAff m => ComponentSpec State Query Action Slots Input Output m
componentSpec = 
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval 
        { handleAction = handleAction
        , handleQuery = handleQuery 
        })
    }
  where
    handleAction :: Action -> HalogenM State Action Slots Output m Unit
    handleAction = outputOnCountChange <<< case _ of
      SetValue str ->
        for_ (fromString str) \i ->
          _count .= i
      Double ->
        _count *= 2
      Quadruple -> do
        handleAction Double
        handleAction Double

    handleQuery :: forall a. Query a -> HalogenM State Action Slots Output m (Maybe a)
    handleQuery = outputOnCountChange <<< case _ of
      Increment next -> do
        _count += 1
        pure (Just next)
      Decrement next -> do
        _count -= 1
        pure (Just next)
      GetCount f -> do
        Just <<< f <$> use (_count)
      DoNothing next-> 
        pure (Just next)
      Ponder ms next -> do
        _isPondering .= true
        liftAff (delay ms)
        _isPondering .= false
        H.raise FinishedPondering
        pure (Just next)

    outputOnCountChange :: forall a. HalogenM State Action Slots Output m a -> HalogenM State Action Slots Output m a
    outputOnCountChange ma = do
      count <- use _count
      a <- ma
      count' <- use _count
      when (count /= count') $
        H.raise CountChanged
      when (count' == 10) do 
        H.raise Equals10
      pure a

initialState :: Input -> State
initialState _ = { count: 0, isPondering: true }

render :: forall m. State -> ComponentHTML Action Slots m
render state = HH.div_
  [ HH.input
    [ HE.onValueChange SetValue ]
  , HH.button [ HE.onClick (const Double) ] [ HH.text "Double" ]
  , HH.button [ HE.onClick (const Quadruple) ] [ HH.text "Quadruple" ]
  , HH.h2_ [ HH.text ("count = " <> show state.count) ]
  , HH.text (if state.isPondering then "Pondering..." else "")
  ]

