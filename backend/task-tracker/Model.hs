{-# LANGUAGE InstanceSigs #-}

module Model(RuntimeConfig(..), ApplicationConfig(..)) where

import Data.Aeson (FromJSON)

import Database.PostgreSQL.Simple ( ToRow, FromRow, Connection )

import Network.HTTP.Types()
import GHC.Generics ( Generic )
import Conferer (FromConfig, DefaultConfig(configDef))  

data RuntimeConfig = RtConfig {cfg :: ApplicationConfig, dbConnection :: Connection} deriving Generic
data ApplicationConfig = AppCfg { port :: Int, 
                                  dbstring :: String, 
                                  schema :: String, 
                                  usertable :: String, 
                                  tasktable :: String
                                } deriving (Show, Generic)
                                
instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 "" "asyncarch" "ttusers" "tasks"             

data User = User { uuid :: String, fullname :: String} deriving (Show, Generic, FromJSON, ToRow, FromRow) 

