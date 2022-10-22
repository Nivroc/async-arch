{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model(RuntimeConfig(..), PostgresSettings(..), ApplicationConfig(..), Task(..), User(..), AuditLogEntry(..), WorkerAccount(..), uuidTask, uuidUser) where

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
import Data.Time (UTCTime)


data RuntimeConfig = RtConfig { cfg :: ApplicationConfig, dbConnection :: Connection, pubChan :: AMQP.Channel, consumeConn :: AMQP.Connection } deriving Generic
instance DBConnect RuntimeConfig where
  schematable :: RuntimeConfig -> (RuntimeConfig -> String) -> String
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . postgres . cfg &&& tbl) $ rt
  connection = dbConnection

data PostgresSettings = PSettings { dbstring :: String, schema :: String, debit :: String, credit :: String, taskcost :: String, acusers :: String, audit :: String } deriving (Show, Generic)
instance FromConfig PostgresSettings
instance DefaultConfig PostgresSettings where
  configDef :: PostgresSettings
  configDef = PSettings "" "asyncarch" "users" "" "" "" ""
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

uuidTask :: Lens' Task UUID
uuidTask = lens (uuid :: Task -> UUID) (\tsk newuuid -> tsk { uuid = newuuid } :: Task)

data User = User { uuid :: UUID, roles :: [Role], fullname :: Maybe String, email :: String } deriving (Show, Generic, FromJSON, ToRow, FromRow) 

uuidUser :: Lens' User UUID
uuidUser = lens (uuid :: User -> UUID) (\usr newuuid -> usr { uuid = newuuid } :: User)

data AuditLogEntry = AuditLogEntry { _uuid :: UUID, 
                                     _title :: String, 
                                     _jira_id :: String,
                                     _userid :: UUID, 
                                     _description :: String, 
                                     _amount :: Int, 
                                     _ts :: UTCTime } deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 

data WorkerAccount = WorkerAccount {balance :: Int, log :: [AuditLogEntry]} deriving (Show, Generic, FromJSON, ToJSON)                                     
