{-# LANGUAGE InstanceSigs #-}

module Model(RuntimeConfig(..), ApplicationConfig(..), User(..), Task(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID
import Database.PostgreSQL.Simple ( ToRow, FromRow, Connection )
import Network.HTTP.Types()
import GHC.Generics ( Generic )
import Conferer (FromConfig, DefaultConfig(configDef))  
import Common
import Control.Arrow ((&&&))
import Network.AMQP (Channel)
import Rabbit

data RuntimeConfig = RtConfig {cfg :: ApplicationConfig, dbConnection :: Connection, rmqchan :: Channel } deriving Generic
instance DBConnect RuntimeConfig where
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . cfg &&& tbl) $ rt
  connection = dbConnection
data ApplicationConfig = AppCfg { port :: Int, 
                                  dbstring :: String, 
                                  schema :: String, 
                                  usertable :: String, 
                                  tasktable :: String,
                                  authid :: String,
                                  authsecret :: String,
                                  userhub :: UserEventHub
                                } deriving (Show, Generic)
                                
instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 "" "asyncarch" "ttusers" "tasks" "" "" configDef          

data User = User { usruuid :: UUID, fullname :: Maybe String } deriving (Show, Generic, FromJSON, ToRow, FromRow) 

data Task = Task { taskuuid :: Maybe UUID, id :: String, name :: String, desc :: String, open :: Bool, assignee :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 

