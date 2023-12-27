module Schema.TaskMessage(TaskMessageV1(..), TaskMessageV2(..)) where
    
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow, ToRow)


data TaskMessageV1 = TaskMessageV1 { uuid_v1 :: Maybe UUID, name_v1 :: String, description_v1 :: String, open_v1 :: Bool, assignee_v1 :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON) 

data TaskMessageV2 = TaskMessageV2 { uuid_v2 :: Maybe UUID, title_v2 :: String, jira_id_v2 :: String, description_v2 :: String, open_v2 :: Bool, assignee_v2 :: UUID } 
  deriving (Show, Generic, FromJSON, ToRow, FromRow, ToJSON)   
