{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model(RuntimeConfig(..), PostgresSettings(..), ApplicationConfig(..), User(..), Task(..), uuidTask, uuidUser) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID
import Database.PostgreSQL.Simple ( ToRow, FromRow, Connection )
import Network.HTTP.Types()
import GHC.Generics ( Generic )
import Conferer (FromConfig, DefaultConfig(configDef))  
import Common
import Control.Arrow ((&&&))
import qualified Network.AMQP as AMQP
import Rabbit
import Lens.Micro (Lens', lens)

data RuntimeConfig = RtConfig { cfg :: ApplicationConfig, dbConnection :: Connection, pubChan :: AMQP.Channel} deriving Generic
instance DBConnect RuntimeConfig where
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . postgres . cfg &&& tbl) $ rt
  connection = dbConnection

data PostgresSettings = PSettings { dbstring :: String, schema :: String, usertable :: String, tasktable :: String, pagination :: Int } deriving (Show, Generic)
instance FromConfig PostgresSettings
instance DefaultConfig PostgresSettings where
  configDef :: PostgresSettings
  configDef = PSettings "" "asyncarch" "users" "" 1
data ApplicationConfig = AppCfg { port :: Int, 
                                  postgres :: PostgresSettings,
                                  authid :: String,
                                  authsecret :: String,
                                  userhub :: UserEventHub,
                                  taskhub :: TaskEventHub
                                } deriving (Show, Generic)
                                                             
instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 configDef "" "" configDef configDef          

data User = User { uuid :: UUID, roles :: [Role], fullname :: Maybe String } deriving (Show, Generic, FromJSON, ToRow, FromRow) 

uuidUser :: Lens' User UUID
uuidUser = lens (uuid :: User -> UUID) (\usr newuuid -> usr { uuid = newuuid } :: User)

data Task = Task { uuid :: Maybe UUID, id :: String, name :: String, description :: String, open :: Bool, assignee :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 

uuidTask :: Lens' Task (Maybe UUID)
uuidTask = lens (uuid :: Task -> Maybe UUID) (\tsk newuuid -> tsk { uuid = newuuid } :: Task)

