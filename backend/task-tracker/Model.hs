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

data RuntimeConfig = RtConfig {cfg :: ApplicationConfig, dbConnection :: Connection} deriving Generic
instance DBConnect RuntimeConfig where
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . cfg &&& tbl) $ rt
  connection = dbConnection
data ApplicationConfig = AppCfg { port :: Int, 
                                  dbstring :: String, 
                                  schema :: String, 
                                  usertable :: String, 
                                  tasktable :: String,
                                  authid :: String,
                                  authsecret :: String
                                } deriving (Show, Generic)
                                
instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 "" "asyncarch" "ttusers" "tasks" "" ""            

data User = User { usruuid :: UUID, fullname :: String } deriving (Show, Generic, FromJSON, ToRow, FromRow) 

data Task = Task { taskuuid :: Maybe UUID, id :: String, name :: String, desc :: String, open :: Bool, assignee :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 

