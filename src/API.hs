{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

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

data EventType = WakeUp | Eat | RockOut

data family Payload (e :: EventType)

data instance (Payload 'WakeUp) = PayloadWakeUp
  deriving stock (Show, Generic)

newtype instance (Payload 'Eat) = PayloadEat { meal :: Meal }
  deriving stock (Show, Generic)

data instance (Payload 'RockOut) = PayloadRockOut { song :: Song, duration :: Duration }
  deriving stock (Show, Generic)

instance Aeson.ToJSON (Payload 'WakeUp)

instance Aeson.FromJSON (Payload 'WakeUp)

instance Aeson.ToJSON (Payload 'Eat)

instance Aeson.FromJSON (Payload 'Eat)

instance Aeson.ToJSON (Payload 'RockOut)

instance Aeson.FromJSON (Payload 'RockOut)

data Event where
  MkEvent :: Aeson.ToJSON (Payload et) => Payload (et :: EventType) -> Event

instance Aeson.ToJSON Event where
  toJSON :: Event -> Aeson.Value
  toJSON (MkEvent et) = Aeson.toJSON et

--------------------------------------------------------------------------------

type Req  = Servant.ReqBody '[Servant.JSON] Aeson.Value
type Resp = Servant.Post '[Servant.JSON] Event

type EventAPI =
  "api" :> "event" :>
             ( "wake-up"  :> Req :> Resp
          :<|> "eat"      :> Req :> Resp
          :<|> "rock-out" :> Req :> Resp
             )

importEvent :: forall (et :: EventType) . (Aeson.FromJSON (Payload et), Aeson.ToJSON (Payload et)) => Aeson.Value -> Servant.Handler Event
importEvent blob =
  case Aeson.fromJSON blob of
    Aeson.Error err -> fail err
    Aeson.Success (res :: Payload et) -> pure (MkEvent res)

wakeUp :: Aeson.Value -> Servant.Handler Event
wakeUp = importEvent @WakeUp

eat :: Aeson.Value -> Servant.Handler Event
eat = importEvent @Eat

rockOut :: Aeson.Value -> Servant.Handler Event
rockOut = importEvent @RockOut

eventServer :: Servant.Application
eventServer = Servant.serve (Proxy @EventAPI) $ wakeUp :<|> eat :<|> rockOut
