{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (fromText)
import           Data.Maybe (listToMaybe)
import           Data.Aeson.Types (ToJSON(..))
import           Data.Int (Int64)

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Applicative ( Applicative(..) )
import           Web.Scotty.Trans
import           Network.HTTP.Types ( status500, status400 )
import           Network.AMQP
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL )
import           Database.PostgreSQL.Simple.Types (Only(..))
import           Control.Monad.IO.Unlift
import           Control.Exception.Lifted
import           Control.Concurrent.QSem.Lifted
import           Control.Monad.Trans.Control ( MonadBaseControl )
import           Control.Monad.Trans.State.Strict (StateT (StateT), execStateT)
import           Database
import           Model hiding (uuid)
import           Common
import           Rabbit (setupRabbit, getConnection, UserEventHub (..), TaskEventHub (..))
import           Auth
import           Control.Arrow ((&&&))
import           Data.Aeson (decode, encode)
import           Lens.Micro ((?~))



main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/tasktracker.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)

-- TODO: вместо UnliftIO предложить в resourcet инстанс MonadBaseControl IO (ResourceT IO) 
-- TODO: из чувства перфекционизма хорошо бы ловить IO эксепшены и nackать сообщения 
registerUser :: (MonadIO m, MonadReader RuntimeConfig m, MonadUnliftIO m) => (Message, Envelope) -> m ()
registerUser (msg, env) = do unl <- askRunInIO
                             liftIO $ do putStrLn $ "received message: " <> show (decode (msgBody msg) :: Maybe User)
                                         (unl . addUser) =<< maybe (nackEnv env *> fail "decoding problem") pure (decode $ msgBody msg)
                                         ackEnv env

program :: (MonadReader ApplicationConfig m, MonadResource m) => m ()
program = do config <- ask
             liftIO $ print config
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring $ postgres config)) ((*> Prelude.putStrLn "connection closed") . close)
             let rabbitCfgPath = "./configs/rabbitmq.properties"
             setupRabbit rabbitCfgPath
             (pubConn, consumeConn) <- liftA2 (,) (getConnection rabbitCfgPath) (getConnection rabbitCfgPath)
             (_, publishChan) <- allocate (openChannel pubConn) closeChannel
             (_, consumeChan) <- allocate (openChannel consumeConn) closeChannel
             let runtime = RtConfig config conn publishChan
             liftIO $ consumeMsgs consumeChan (T.pack . ttqueue $ userhub config) Ack ((`runReaderT` runtime) . registerUser )
             shuffleOngoing <- liftIO $ newQSem 1
             liftIO $ putStrLn "Configuration finished"
             scottyT (port config) (`runReaderT` runtime) (Main.routes shuffleOngoing)

routes :: (MonadBaseControl IO m, MonadReader RuntimeConfig m, MonadIO m) => QSem -> ScottyT TL.Text m ()
routes sem = do
            midware
            defaultHandler $ \str -> status status500 *> json str

            let login = uncurry verifyAgainstAuthService =<< asks ((authid &&& authsecret) . cfg)

            get "/assigned/:userid" $ do tok <- login
                                         hasRights <- hasRole tok Worker
                                         if hasRights
                                         then json =<< ((toJSON <$>) . fetchTasks ) =<< (maybe (actionErr "task id sent is not uuid" status400) pure . fromText) =<< param "userid"
                                         else actionErr "Your beak is not pointy enough to do that" status400

            put "/shuffle" $ do tok <- login
                                hasRights <- liftA2 (||) (hasRole tok Admin) (hasRole tok Manager)
                                if hasRights
                                then bracket_ (waitQSem sem) (signalQSem sem) ( bracket (count =<< shuffleView "test") (dropTempTable . fst) (uncurry sendUpdates) )
                                else actionErr "Your beak is not pointy enough to do that" status400

            put "/close/:taskid" $ do _ <- login
                                      tid <- maybe (actionErr "task id sent is not uuid" status500) pure . fromText =<< param "taskid"
                                      rowsAff <- closeTask tid
                                      if rowsAff == 0 then text "No such task exists"
                                      else sendTaskMessage crtkey tid *> text "Success" -- <* эвент

            post "/create" $ do _ <- login
                                task :: Task <- jsonData
                                newUUID <- liftIO nextRandom
                                userPresent <- checkUserExistsTask (assignee task) >>= maybe (actionErr "Assignee not found" status500) pure . listToMaybe . (fromOnly <$>)
                                if userPresent then do
                                  let enrichedTask = (uuidTask ?~ newUUID) task { open = True }
                                  addTask enrichedTask
                                  sendTaskMessage crtkey enrichedTask
                                  text "Success"
                                else actionErr "Assignee not found" status500

sendTaskMessage :: (ToJSON a, MonadReader RuntimeConfig m, MonadIO m) => (TaskEventHub -> String) -> a -> m ()
sendTaskMessage k task = do (exc, routingKey) <- asks ((T.pack . taskexchange &&& T.pack . k) . taskhub . cfg)
                            chan <- asks pubChan
                            void $ liftIO $ publishMsg chan exc routingKey (newMsg {msgBody = encode task})

sendUpdates :: (DBConstraints m RuntimeConfig) => String -> Int64 -> m ()
sendUpdates table amountToSend = do
  liftIO $ print $ "sending updates for all in table " <> table <> " with " <> show amountToSend <> " rows"
  pag <- asks (pagination . postgres . cfg)
  let batches = (fromIntegral amountToSend :: Int) `quot` pag
  let getAndSendBatch = StateT (\i -> (, i+1) <$> ((sendTaskMessage updkey `traverse`) =<< fetchNumTasks i table))
  void $ execStateT (replicateM batches getAndSendBatch) 1



