{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import           Data.Text.Encoding                                          as T ( encodeUtf8)
import           Data.Text                                                   as T ( pack, unpack )
import qualified Data.Text.Lazy                                              as TL
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (fromText)
import           Data.Maybe (listToMaybe)
import           Data.Aeson (fromJSON, Result (Error, Success))
import           Data.Aeson.Types (ToJSON(..))
import qualified Data.ByteString.Lazy.Char8                                  as BL
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
import           Network.AMQP
import qualified Web.JWT                                                     as J
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL )
import           Database.PostgreSQL.Simple.Types (Only(..))
import           Database
import           Model
import           Common
import           Rabbit (setupRabbit, UserEventHub (queue))
import           Control.Monad.IO.Unlift
import qualified Data.Text.Lazy.Encoding as TL
import           Control.Exception.Lifted
import           Control.Concurrent.QSem.Lifted
import           Control.Monad.Trans.Control
import Data.Int (Int64)

main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/tasktracker.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)

-- TODO: вместо UnliftIO предложить в resourcet инстанс MonadBaseControl IO (ResourceT IO) 
-- TODO: из чувства перфекционизма хорошо бы ловить IO эксепшены и nackать сообщения 
registerUser :: (MonadIO m, MonadReader RuntimeConfig m, MonadUnliftIO m) => (Message, Envelope) -> m ()
registerUser (msg, env) = do unl <- askRunInIO
                             liftIO $ do putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
                                         unl $ addUser $ TL.unpack . TL.decodeUtf8 . msgBody $ msg
                                         ackEnv env
-- На случай важных переговоров maybe (putStrLn "Message cannot be decoded" *> nackEnv env) (unl . void . addUser)


program :: (MonadReader ApplicationConfig m, MonadResource m) => m ()
program = do config <- ask
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring $ postgres config)) ((*> Prelude.putStrLn "connection closed") . close)
             chan <- setupRabbit "./configs/rabbitmq.properties"
             let runtime = RtConfig config conn chan
             liftIO $ consumeMsgs chan (T.pack . queue $ userhub config) Ack ((`runReaderT` runtime) . registerUser )
             shuffleOngoing <- liftIO $ newQSem 1
             scottyT (port config) (`runReaderT` runtime) (Main.routes shuffleOngoing)

routes :: (MonadBaseControl IO m, MonadReader RuntimeConfig m, MonadIO m) => QSem -> ScottyT TL.Text m ()
routes sem = do 
            midware
            defaultHandler $ \str -> status status500 *> json str

            --Только лишь чекать что он декодится не ок потому что он может истечь или быть залогаутеным? Переделать на поход в auth?
            let auth = do let err = throwError $ ActionError status400 "Error: no 'authorization' header"
                          tokenText <- TL.toStrict . TL.drop 7 <$> (header "authorization" >>= maybe err pure)
                          let er = throwError $ ActionError status422 "Auth JWT cannot be decoded"
                          liftIO $ putStrLn $ T.unpack tokenText
                          secret <- asks $ authsecret . cfg
                          liftIO $ putStrLn  secret
                          maybe er pure (J.decodeAndVerifySignature (J.toVerify . J.hmacSecret . T.pack $ secret ) tokenText)

            let err mess = throwError $ ActionError status400 (TL.pack mess)

            let hasRole token role = do roles <- maybe (err "No roles found in claims") pure . Data.Map.lookup "roles" . J.unClaimsMap . J.unregisteredClaims . J.claims $ token
                                        case fromJSON roles :: Result [Role] of
                                            Error s -> err s
                                            Success ros -> pure $ elem role ros

            --TODO: отсылаем эвент
            get "/assigned/:userid" $ do tok <- auth
                                         hasRights <- hasRole tok Worker
                                         if hasRights
                                         then json =<< ((toJSON <$>) . fetchTasks ) =<< (maybe (err "task id sent is not uuid") pure . fromText) =<< param "userid"
                                         else err "Your beak is not pointy enough to do that"

            put "/shuffle" $ do tok <- auth
                                hasRights <- liftA2 (||) (hasRole tok Admin) (hasRole tok Manager)
                                if hasRights 
                                then bracket_ (waitQSem sem) (signalQSem sem) ( bracket (count =<< shuffleView "test") (dropTempTable . fst) (uncurry sendUpdates) ) 
                                else err "Your beak is not pointy enough to do that"

            put "/close/:taskid" $ do _ <- auth
                                      tid <- maybe (err "task id sent is not uuid") pure . fromText =<< param "taskid"
                                      rowsAff <- closeTask tid
                                      if rowsAff == 0 then text "No such task exists" else text "Success" -- <* эвент

            post "/create" $ do _ <- auth
                                task :: Task <- jsonData
                                newUUID <- liftIO nextRandom
                                userPresent <- checkUserExistsTask (assignee task) >>= maybe (err "Assignee not found") pure . listToMaybe . (fromOnly <$>)
                                if userPresent then do
                                  _ <- addTask $ task { taskuuid = Just newUUID, open = True }
                                  text "Success"
                                else err "Assignee not found"


sendUpdates :: MonadIO m => String -> Int64 -> m ()
sendUpdates table rows = liftIO $ print $ "sending updates for all in table " <> table <> " with " <> show rows <> " rows"