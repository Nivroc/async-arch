{-# LANGUAGE InstanceSigs #-}

module Model(RuntimeConfig(..), PostgresSettings(..), ApplicationConfig(..), Task(..)) where

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

data RuntimeConfig = RtConfig { cfg :: ApplicationConfig, dbConnection :: Connection, pubChan :: AMQP.Channel, consumeConn :: AMQP.Connection } deriving Generic
instance DBConnect RuntimeConfig where
  schematable :: RuntimeConfig -> (RuntimeConfig -> String) -> String
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . postgres . cfg &&& tbl) $ rt
  connection = dbConnection

data PostgresSettings = PSettings { dbstring :: String, schema :: String, debit :: String, credit :: String, taskcost :: String } deriving (Show, Generic)
instance FromConfig PostgresSettings
instance DefaultConfig PostgresSettings where
  configDef :: PostgresSettings
  configDef = PSettings "" "asyncarch" "users" "" ""
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

data Task = Task { uuid :: UUID, 
                   title :: String, 
                   jira_id :: String, 
                   cost :: Maybe Int, 
                   reward :: Maybe Int ,
                   description :: String, 
                   open :: Bool, 
                   assignee :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 
