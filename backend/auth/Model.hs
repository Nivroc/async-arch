{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Model(RuntimeConfig (..), ApplicationConfig(..), PostgresSettings(..), User(..), Role, Creds(..), Token(..), ClientApp(..), uuidL, validateUser, validateRoles) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe ( isJust )
import Data.Char (isDigit)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T

import Database.PostgreSQL.Simple ( ToRow, FromRow, Connection )

import Network.HTTP.Types()
import Text.Regex ( matchRegex, mkRegex )
import Validation( failureIf, failureUnless, validateAll, Validation )
import GHC.Generics ( Generic )
import Lens.Micro
import Conferer (FromConfig, DefaultConfig(configDef))
import Common
import Control.Arrow ((&&&))
import Network.AMQP (Channel)
import Rabbit

data RuntimeConfig = RtConfig { cfg :: ApplicationConfig, dbConnection :: Connection, rmqchan :: Channel } deriving (Generic)
data ApplicationConfig = AppCfg { port :: Int, 
                                  postgres :: PostgresSettings,
                                  tokenexpiration :: Int,
                                  userhub :: UserEventHub
                                } deriving (Show, Generic)

data PostgresSettings = PSettings {dbstring :: String, schema :: String, blacklist :: String, usertable :: String, clienttable :: String } deriving (Show, Generic)

instance DBConnect RuntimeConfig where
  schematable :: RuntimeConfig -> (RuntimeConfig -> String) -> String
  schematable rt tbl = uncurry ((<>) . (<> ".")) . (schema . postgres . cfg &&& tbl) $ rt
  connection = dbConnection

instance FromConfig PostgresSettings
instance DefaultConfig PostgresSettings where
  configDef :: PostgresSettings
  configDef = PSettings "" "asyncarch" "users" "blctokens" "registeredapps"

instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 configDef 0 configDef


data User = User { uuid :: Maybe String, login :: String, email :: String, secret :: String, roles :: [Role] } deriving (Show, Generic, FromJSON, ToJSON, ToRow, FromRow)
uuidL :: Lens' User (Maybe String)
uuidL = lens uuid (\usr newuuid -> usr { uuid = newuuid })

data ClientApp = ClientApp { clientid :: String, clientSecret :: T.Text } deriving (Show, Generic, ToRow, FromRow, FromJSON)  
data Creds = Creds { sub :: String, username :: String, password :: String, app :: ClientApp, redirect :: T.Text } deriving (Show, Generic, FromJSON)
data Token = Token { token :: T.Text, expiresIn :: Int, assignedRoles :: [Role]} deriving (Show, Generic, FromJSON, ToJSON)
  
data UserEntryValidationError = EmptyName | ShortPassword | NoDigitPassword | InvalidEmail | NoRolesSet | EmptySecret deriving Show
type UserValidation a = a -> Validation (NonEmpty UserEntryValidationError) a

validateUser :: UserValidation User
validateUser u = User (uuid u) <$> validateName (login u) <*> validateEmail (email u) <*> validatePassword (secret u) <*> validateRoles (roles u)

validateName :: UserValidation String
validateName n = n <$ failureIf (null n) EmptyName

validatePassword :: UserValidation String
validatePassword = validateAll [ (`failureIf`     ShortPassword)   . (< 8) . length
                               , (`failureUnless` NoDigitPassword) . any isDigit ]

validateEmail :: UserValidation String
validateEmail e = e <$ failureUnless (isJust . matchRegex (mkRegex "^[a-zA-Z0-9\\.\\+\\-]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+$") $ e) InvalidEmail

validateRoles :: UserValidation [Role]
validateRoles e = e <$ failureIf (null e) NoRolesSet