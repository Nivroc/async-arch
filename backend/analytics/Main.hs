{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import           GHC.Generics ( Generic )
import           Data.Text.Encoding                                          as T ( encodeUtf8)
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL
import           Data.Aeson (decode, FromJSON)
import           Data.Time (UTCTime(..))

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Applicative (Applicative(..), liftA3)
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control
import           Control.Arrow ((&&&))
import           Web.Scotty.Trans
import           Network.HTTP.Types(status500, status401)
import           Network.AMQP hiding (flow)
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL, withTransaction )
import           Model
import           Common
import           Database
import           Rabbit (getConnection, TaskEventHub (..))
import           Control.Exception (catches, Handler (..), throwIO, SomeException)
import           Auth (verifyAgainstAuthService, hasRole, actionErr)
import           Data.List.NonEmpty (groupBy, partition)
import           Prelude hiding (head)


main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/analytics.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)


program :: (MonadReader ApplicationConfig m, MonadResource m, MonadUnliftIO m) => m ()
program = do config <- ask
             let rabbitCfgPath = "./configs/rabbitmq.properties"
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring $ postgres config)) ((*> Prelude.putStrLn "connection closed") . close)
             consumeC <- getConnection rabbitCfgPath
             let runtime = RtConfig config conn consumeC
             runReaderT setupConsumers runtime
             scottyT (port config) (`runReaderT` runtime) Main.routes

data TimeRange = TimeRange {from_time :: UTCTime, to_time :: UTCTime} deriving (Show, Generic, FromJSON)

routes :: (MonadBaseControl IO m, MonadReader RuntimeConfig m, MonadIO m) => ScottyT TL.Text m ()
routes = do
    midware
    defaultHandler $ \str -> status status500 *> json str

    let login = uncurry verifyAgainstAuthService =<< asks ((authid &&& authsecret) . cfg)

    let flow foo agg = do accessGranted <- flip hasRole Admin =<< login
                          if accessGranted then do
                              timeRange :: TimeRange <- jsonData
                              auditLog <- filter (foo timeRange) <$> allAuditLog
                              text . TL.pack . show $ agg (_amount <$> auditLog)
                          else actionErr "Your beak is not pointy enough to do that" status401

    post "/total-earned/" $ flow (\timeRange -> liftA2 (&&) (> from_time timeRange) (< to_time timeRange) . _ts) sum

    post "/most-expensive/" $ flow (\timeRange -> liftA3 (\ a b c -> a && b && c) ((> from_time timeRange) . _ts) ((< to_time timeRange) . _ts) (not . _open)) maximum

    get "/papugs-in-poverty/" $ do
        accessGranted <- flip hasRole Admin =<< login
        if accessGranted then do
            bomzi <- sum . filter (< 1) . (calculate . brk <$>) . groupedByUser <$> allAuditLog
            text . TL.pack . show $ "Number on papugs in-debt = " <> show bomzi
        else actionErr "Your beak is not pointy enough to do that" status401
        where groupedByUser = groupBy ((. _userid) . (==) . _userid)
              brk = partition ((True ==) . _isCredit)
              calculate (c, d) = summ d - summ c
              summ transhes = sum (_amount <$> transhes)



type MessageReact m a = (a -> m ()) -> (Message, Envelope) -> m ()
type ConsumerConstraints r m a = (MonadReader r m, MonadResource m, MonadUnliftIO m)

-- На каждый по каналу, все каналы в одном коннекшене. Все действия в цепочкe Клейсли выполняются в рамках одной бд транзакции
setupConsumers :: ( ConsumerConstraints RuntimeConfig m a) =>m ()
setupConsumers = do createConsumer (crtqueue . taskhub) (addEntry tasks)
--                 where voidPair = arr (void . uncurry (liftA2 (,)))

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





