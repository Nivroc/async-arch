module Schema.UserMessage( UserMessage(..) ) where

import Common (Role)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (ToRow, FromRow)


data UserMessage = UserMessage { uuid :: Maybe String, login :: String, email :: String, secret :: String, roles :: [Role] } deriving (Show, Generic, FromJSON, ToJSON, ToRow, FromRow)
