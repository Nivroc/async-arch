{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (fromText)
import           Data.Maybe (listToMaybe)
import           Data.Aeson (fromJSON, Result (Error, Success))
import           Data.Aeson.Types (ToJSON(..))
import qualified Data.Map
                                            
import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Monad.Except (throwError)

import           Control.Applicative (Applicative(..))
import           Web.Scotty.Trans
import           Web.Scotty.Internal.Types (ActionError(..))
import           Network.HTTP.Types(status500, status400, status422)
import qualified Web.JWT                                                     as J       
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL )
import           Database.PostgreSQL.Simple.Types (Only(..))

import           Database
import           Model
import           Common

main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/tasktracker.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)

program :: (MonadIO m, MonadReader ApplicationConfig m, MonadResource m) => m ()
program = do config <- ask
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring config)) ((*> Prelude.putStrLn "connection closed") . close)
             let runtime = RtConfig config conn
             liftIO $ print config
             scottyT (port config) (`runReaderT` runtime) Main.routes

routes :: (MonadReader RuntimeConfig m, MonadIO m) => ScottyT TL.Text m ()
routes = do midware
            defaultHandler $ \str -> status status500 *> json str

            let auth = do let err = throwError $ ActionError status400 "Error: no 'authorization' header"
                          tokenText <- TL.toStrict . TL.drop 7 <$> (header "authorization" >>= maybe err pure)
                          let er = throwError $ ActionError status422 "Auth JWT cannot be decoded"
                          secret <- asks $ authsecret . cfg
                          maybe er pure (J.decodeAndVerifySignature (J.toVerify . J.hmacSecret . T.pack $ secret ) tokenText)

            let err mess = throwError $ ActionError status400 (TL.pack mess)

            let hasRole role = do roles <- maybe (err "No roles found in claims") pure . Data.Map.lookup "roles" . J.unClaimsMap . J.unregisteredClaims . J.claims =<< auth
                                  case fromJSON roles :: Result [Role] of
                                      Error s -> err s
                                      Success ros -> pure $ elem role ros

            --TODO: отсылаем кафка эвент
            get "/assigned/:userid" $ do hasRights <- hasRole Worker
                                         if hasRights 
                                         then json =<< ((toJSON <$>) . fetchTasks ) =<< (maybe (err "task id sent is not uuid") pure . fromText) =<< param "userid" 
                                         else err "Your beak is not pointy enough to do that"

            put "/shuffle" $ do hasRights <- liftA2 (||) (hasRole Admin) (hasRole Manager)
                                if hasRights then reassignTasks else err "Your beak is not pointy enough to do that"  -- и послать эвенты в кафку   

            put "/close/:taskid" $ do _ <- auth
                                      tid <- maybe (err "task id sent is not uuid") pure . fromText =<< param "taskid"
                                      rowsAff <- closeTask tid
                                      if rowsAff == 0 then text "No such task exists" else text "Success" -- <* кафка эвент

            post "/create" $ do _ <- auth
                                task :: Task <- jsonData
                                newUUID <- liftIO nextRandom
                                userPresent <- checkUserExistsTask (assignee task) >>= maybe (err "Assignee not found") pure . listToMaybe . (fromOnly <$>)
                                if userPresent then do
                                  _ <- addTask $ task { taskuuid = Just newUUID, open = True }
                                  -- отсылаем кафка эвент
                                  text "Success"
                                else err "Assignee not found"

--TODO: get kafka event and add user
registerUser = undefined

--TODO: get kafka event and remove user
removeUser = undefined
