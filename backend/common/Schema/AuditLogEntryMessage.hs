module Schema.AuditLogEntryMessage (AuditLogEntryMessage (..)) where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data AuditLogEntryMessage = AuditLogEntryMessage { 
  e_uuid :: UUID, 
  e_title :: String, 
  e_jira_id :: String,
  e_userid :: UUID, 
  e_amount :: Int, 
  e_open :: Bool,
  e_isCredit :: Bool,
  e_ts :: UTCTime 
} deriving (Show, Generic, FromJSON, ToJSON) 
