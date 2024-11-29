{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( EventAPI,
    eventServer,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Servant ((:<|>) (..), (:>))
import Servant qualified

--------------------------------------------------------------------------------

type Meal = String

type Song = String

type Duration = Int

data Event = WakeUp | Eat Meal | RockOut Song Duration
  deriving stock Show

instance Aeson.FromJSON Event where
  parseJSON = Aeson.withObject "Event" $ \ob -> do
      event :: String <- ob .: "event"
      case event of
        "wakeUp" -> parseWakeUp
        "eat" -> parseEat ob 
        "rockOut" -> parseRockOut ob
        _ -> fail "Invalid Event"
    where
      parseWakeUp = pure WakeUp

      parseEat ob = do
        meal <- ob .: "meal"
        pure (Eat meal)

      parseRockOut ob = do
        song <- ob .: "song"
        duration <- ob .: "duration"
        pure (RockOut song duration)

--------------------------------------------------------------------------------

type Req  = Servant.ReqBody '[Servant.JSON] Aeson.Value
type Resp = Servant.Post '[Servant.JSON] String

type EventAPI =
  "api" :> "event" :>
             ( "wake-up"  :> Req :> Resp
          :<|> "eat"      :> Req :> Resp
          :<|> "rock-out" :> Req :> Resp
             )

importEvent :: Aeson.Value -> Servant.Handler String
importEvent blob =
  case Aeson.fromJSON @Event blob of
    Aeson.Error err -> fail err
    Aeson.Success res -> pure (show res)

wakeUp :: Aeson.Value -> Servant.Handler String
wakeUp = importEvent

eat :: Aeson.Value -> Servant.Handler String
eat = importEvent

rockOut :: Aeson.Value -> Servant.Handler String
rockOut = importEvent

eventServer :: Servant.Application
eventServer = Servant.serve (Proxy @EventAPI) $ wakeUp :<|> eat :<|> rockOut
