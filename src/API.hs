{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( EventAPI,
    eventServer,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import GHC.Generics
import Servant ((:<|>) (..), (:>))
import Servant qualified

--------------------------------------------------------------------------------

type Meal = String

type Song = String

type Duration = Int

data PayloadWakeUp = PayloadWakeUp
  deriving stock (Show, Generic)

newtype PayloadEat = PayloadEat { meal :: Meal }
  deriving stock (Show, Generic)

data PayloadRockOut = PayloadRockOut { song :: Song, duration :: Duration }
  deriving stock (Show, Generic)

instance Aeson.FromJSON PayloadWakeUp
instance Aeson.FromJSON PayloadEat
instance Aeson.FromJSON PayloadRockOut

data Event =
   EventWakeUp PayloadWakeUp
 | EventEat PayloadEat
 | EventRockOut PayloadRockOut
  deriving Show

--------------------------------------------------------------------------------

type Req  = Servant.ReqBody '[Servant.JSON] Aeson.Value
type Resp = Servant.Post '[Servant.JSON] String

type EventAPI =
  "api" :> "event" :>
             ( "wake-up"  :> Req :> Resp
          :<|> "eat"      :> Req :> Resp
          :<|> "rock-out" :> Req :> Resp
             )

importEvent :: forall e. (Aeson.FromJSON e, Show e) => Proxy e -> Aeson.Value -> Servant.Handler String
importEvent _ blob =
  case Aeson.fromJSON @e blob of
    Aeson.Error err -> fail err
    Aeson.Success res -> pure (show res)

wakeUp :: Aeson.Value -> Servant.Handler String
wakeUp = importEvent (Proxy @PayloadWakeUp)

eat :: Aeson.Value -> Servant.Handler String
eat = importEvent (Proxy @PayloadEat)

rockOut :: Aeson.Value -> Servant.Handler String
rockOut = importEvent (Proxy @PayloadRockOut)

eventServer :: Servant.Application
eventServer = Servant.serve (Proxy @EventAPI) $ wakeUp :<|> eat :<|> rockOut
