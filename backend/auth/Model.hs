{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Model(RuntimeConfig (..), ApplicationConfig(..), User(..), Role, Creds(..), Token(..), ClientApp(..), uuidL, validateUser, validateRoles) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe ( isJust )
import Data.Char (isDigit)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T

import Database.PostgreSQL.Simple.FromField ( FromField(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(..) )
import Database.PostgreSQL.Simple ( ToRow, FromRow, Connection )
import Database.PostgreSQL.Simple.Types (PGArray (PGArray, fromPGArray))

import Network.HTTP.Types()
import Text.Regex ( matchRegex, mkRegex )
import Validation( failureIf, failureUnless, validateAll, Validation )
import GHC.Generics ( Generic )
import Lens.Micro
import Conferer (FromConfig, DefaultConfig(configDef))

data RuntimeConfig = RtConfig {cfg :: ApplicationConfig, dbConnection :: Connection} deriving (Generic)
data ApplicationConfig = AppCfg { port :: Int, 
                                  dbstring :: String, 
                                  schema :: String, 
                                  blacklist :: String, 
                                  usertable :: String, 
                                  clienttable :: String,
                                  tokenexpiration:: Int
                                } deriving (Show, Generic)
                        
instance FromConfig ApplicationConfig
instance DefaultConfig ApplicationConfig where
  configDef :: ApplicationConfig
  configDef = AppCfg 8080 "" "asyncarch" "users" "blctokens" "registeredapps" 0

data Role = Worker | Admin | Accountant | Manager deriving (Show, Read, Generic, FromJSON, ToJSON)
data User = User { uuid :: Maybe String, login :: String, email :: String, secret :: String, roles :: [Role] } deriving (Show, Generic, FromJSON, ToRow, FromRow)
uuidL :: Lens' User (Maybe String)
uuidL = lens uuid (\usr newuuid -> usr { uuid = newuuid })

data ClientApp = ClientApp { clientid :: String, clientSecret :: T.Text } deriving (Show, Generic, ToRow, FromRow, FromJSON)  
data Creds = Creds { sub :: String, username :: String, password :: String, app :: ClientApp, redirect :: T.Text } deriving (Show, Generic, FromJSON)
data Token = Token { token :: T.Text, expiresIn :: Int, assignedRoles :: [Role]} deriving (Show, Generic, FromJSON, ToJSON)
  
data UserEntryValidationError = EmptyName | ShortPassword | NoDigitPassword | InvalidEmail | NoRolesSet | EmptySecret deriving Show
type UserValidation a = a -> Validation (NonEmpty UserEntryValidationError) a
instance ToField Role where toField = toField . show
instance FromField Role where fromField f dat = read <$> fromField f dat
instance ToField [Role] where toField = toField . PGArray
instance FromField [Role] where fromField f dat = fromPGArray <$> fromField f dat

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