{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import           GHC.Generics ( Generic )
import           System.Random ( randomRIO )
import           Data.Text.Encoding                                          as T ( encodeUtf8)
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL
import           Data.Aeson (decode, FromJSON, ToJSON, encode)
import           Data.UUID (fromText, UUID)
import           Data.Time (UTCTime(..), getCurrentTime)

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Applicative (Applicative(..), (<|>))
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control
import           Control.Arrow ((&&&), (***))
import           Control.Category ((<<<))
import           Web.Scotty.Trans
import           Network.HTTP.Types(status500, status401)
import           Network.AMQP
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL, withTransaction )
import           Model
import           Common
import           Database
import           Rabbit (setupRabbit, getConnection, TaskEventHub (..), UserEventHub (..), PaymentEventHub (..))
import           Control.Exception (catches, Handler (..), throwIO, SomeException)
import           Auth (verifyAgainstAuthService, hasRole, actionErr)
import           Data.List.NonEmpty (groupBy, NonEmpty, head)
import           Prelude hiding (head)
import           Data.Bifoldable (bisequenceA_)


main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/accounting.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)


program :: (MonadReader ApplicationConfig m, MonadResource m, MonadUnliftIO m, MonadFail m) => m ()
program = do config <- ask
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring $ postgres config)) ((*> Prelude.putStrLn "connection closed") . close)
             let rabbitCfgPath = "./configs/rabbitmq.properties"
             setupRabbit rabbitCfgPath
             (pubConn, consumeC) <- liftA2 (,) (getConnection rabbitCfgPath) (getConnection rabbitCfgPath)
             (_, publishChan) <- allocate (openChannel pubConn) closeChannel
             let runtime = RtConfig config conn publishChan consumeC
             runReaderT setupConsumers runtime
             scottyT (port config) (`runReaderT` runtime) Main.routes

data TimeRange = TimeRange {from_time :: UTCTime, to_time :: UTCTime} deriving (Show, Generic, FromJSON)

routes :: (MonadBaseControl IO m, MonadReader RuntimeConfig m, MonadIO m) => ScottyT TL.Text m ()
routes = do
    midware
    defaultHandler $ \str -> status status500 *> json str

    let login = uncurry verifyAgainstAuthService =<< asks ((authid &&& authsecret) . cfg)

    get "/account/:user" $ do tok <- login
                              hasRights <- hasRole tok Worker
                              uid <- justOrLift status500 "UUID parse error" $ fromText <$> param "user"
                              if hasRights
                              then do time <- liftIO getCurrentTime
                                      let midnightToday = time { utctDayTime = 0 }
                                      auditLog <- filter (( midnightToday > ) . _ts) <$> workerAuditLog uid
                                      json $ WorkerAccount (sum $ _amount <$> auditLog) auditLog
                              else actionErr "Your beak is not dull enough to do that" status401

    get "/total-earned/simple" $ do tok <- login
                                    hasRights <- hasRole tok Admin <|> hasRole tok Manager
                                    if hasRights
                                    then do time <- liftIO getCurrentTime
                                            let midnightToday = time { utctDayTime = 0 }
                                            auditLog <- filter (( midnightToday > ) . _ts) <$> allAuditLog
                                            text . TL.pack . show $ sum (_amount <$> auditLog)
                                    else actionErr "Your beak is not pointy enough to do that" status401

    -- заработанное за любой промежуток. на фронте можно вбить любой рейнж дат. хоть за дни хоть за секунды
    post "/total-earned/" $ do tok <- login
                               hasRights <- hasRole tok Admin <|> hasRole tok Manager
                               timeRange :: TimeRange <- jsonData
                               if hasRights
                               then do auditLog <- filter (liftA2 (&&) (> from_time timeRange) (< to_time timeRange) . _ts) <$> allAuditLog
                                       text . TL.pack . show $ sum (_amount <$> auditLog)
                               else actionErr "Your beak is not pointy enough to do that" status401

    -- предполагается что дергаем по крону раз в день в 23:59:59
    -- (так удобнее всего в рамках нашей учебной системы, в реальной системе этого метода бы не было и был бы учет времени с синком на какой то Google Time)
    put "/close-day/" $ do tok <- login
                           hasRights <- hasRole tok Admin <|> hasRole tok Accountant
                           if hasRights
                           then do time <- liftIO getCurrentTime
                                   let midnightToday = time { utctDayTime = 0 }
                                   let secondToMidnightToday = time { utctDayTime = 86400 }
                                   auditLogForToday <- filter (liftA2 (&&) (> secondToMidnightToday) (< midnightToday) . _ts) <$> allAuditLog
                                   let groupedByUser = groupBy ((. _userid) . (==) . _userid) auditLogForToday
                                   traverse sendEmail =<< (payBalance `traverse` groupedByUser)
                                   text "Success"
                           else actionErr "Your beak is not pointy enough to do that" status401

payBalance :: DBConstraints m RuntimeConfig => NonEmpty AuditLogEntry -> m (UUID, Int)
payBalance auditLog =
    let totalEarned = sum (_amount <$> auditLog) in
    if totalEarned > 0 then addEntry debit "[PAYCHECK]" (_userid $ head auditLog) totalEarned
    else if totalEarned == 0 then pure (_userid $ head auditLog, 0)
    else addEntry credit "[DEBT]" (_userid $ head auditLog) (-totalEarned)

-- TODO: в последнюю очередь
sendEmail :: Monad m => (UUID, Int) -> m ()
sendEmail = undefined

type MessageReact m a = (a -> m ()) -> (Message, Envelope) -> m ()
type ConsumerConstraints r m a = (MonadReader r m, MonadResource m, MonadUnliftIO m)

enrichTaskWithCosts :: MonadIO m => Task -> m Task
enrichTaskWithCosts t = do cst <- randomRIO (10, 20)
                           awrd <- randomRIO (20, 40)
                           return t { cost = Just cst, reward = Just awrd}

-- На каждый по каналу, все каналы в одном коннекшене. Все действия в цепочкe Клейсли выполняются в рамках одной бд транзакции
setupConsumers :: (MonadFail m, ConsumerConstraints RuntimeConfig m a) => m ()
setupConsumers = do createConsumer (crtqueue . taskhub) ((bisequenceA_ <<< ((sendTransaction True . fst) <=< creditUser) &&& checkUserExists . assignee) <=< enrichTaskWithCosts )
                    createConsumer (cltqueue . taskhub) ((bisequenceA_ <<< ((bisequenceA_ <<< sendTransaction False *** closeTask) <=< debitUser) &&& (checkUserExists . assignee)) <=< getTask )
                    createConsumer (updqueue . taskhub) ( bisequenceA_ <<< (((sendTransaction True . fst) <=< creditUser <=< changeAssignee) &&& (checkUserExists . assignee)))
                    createConsumer (acqueue . userhub) addUser
                 where sendTransaction cred = sendMessage (anakey . paymenthub) (anaqueue . paymenthub) (createAuditMessage cred)
                       createAuditMessage cred original = AuditLogEntryEvent (_uuid original) (_title original) (_jira_id original) (_userid original) (_amount original) cred cred (_ts original)

createConsumer :: (Show a, FromJSON a, ConsumerConstraints RuntimeConfig m a) => (ApplicationConfig -> String) -> (a -> m ()) -> m ()
createConsumer queue callback = do
    run <- askRunInIO
    runtime <- ask
    (_, consumeChan) <- allocate (openChannel $ consumeConn runtime) closeChannel
    void $ liftIO $ consumeMsgs consumeChan (T.pack . queue . cfg $ runtime) Ack (run . decodeAndAck callback)

decodeAndAck :: (Show a, FromJSON a, ConsumerConstraints RuntimeConfig m a) => MessageReact m a
decodeAndAck action (msg, env) = do
    unl <- askRunInIO
    conn <- asks dbConnection
    liftIO $ do let dcd = decode $ msgBody msg
                putStrLn $ "received message: " <> show dcd
                (withTransaction conn . unl . action) =<< maybe (nackEnv env *> fail "decoding problem") pure dcd
                ackEnv env
        `catches` [ Handler $ \(e :: ChanThreadKilledException) -> nackEnv env *> throwIO e,
                    Handler $ \(e :: SomeException) -> nackEnv env *> putStrLn ("Unrecognized Exception" <> show e)]


sendMessage :: (ToJSON b, MonadReader RuntimeConfig m, MonadIO m) => (ApplicationConfig -> String) -> (ApplicationConfig -> String) -> (a -> b) -> a -> m a
sendMessage k ex transform mess = do
        (exc, routingKey) <- asks ((T.pack . ex &&& T.pack . k) . cfg)
        chan <- asks pubChan
        liftIO $ publishMsg chan exc routingKey (newMsg {msgBody = encode $ transform mess})
        return mess



