{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import           System.Random ( randomRIO )
import           Data.Text.Encoding                                          as T ( encodeUtf8)
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL
import           Data.Aeson (decode, FromJSON)

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Applicative (Applicative(..))
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control
import           Web.Scotty.Trans
import           Network.HTTP.Types(status500)
import           Network.AMQP
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL, withTransaction )
import           Model
import           Common
import           Database
import           Rabbit (setupRabbit, getConnection, TaskEventHub (..))
import           Control.Exception (catches, Handler (..), throwIO, SomeException)



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

routes :: (MonadBaseControl IO m, MonadReader RuntimeConfig m, MonadIO m) => ScottyT TL.Text m ()
routes = do
    midware
    defaultHandler $ \str -> status status500 *> json str

            --TODO: отсылаем эвент
    get "/log/:user" undefined -- аудит лог отдельного юзера

    put "/balance/:user" undefined -- сколько денег у юзера

    get "/total-earned/simple" undefined -- показываем заработанное всего за последний день

    post "/total-earned/" undefined -- если успею то за выбранный временной период

    put "/close-day/" undefined -- вообще по времени, этот метод нужен для тестирования


type MessageReact m a = (a -> m ()) -> (Message, Envelope) -> m ()
type ConsumerConstraints r m a = (MonadReader r m, MonadResource m, MonadUnliftIO m)

enrichTaskWithCosts :: MonadIO m => Task -> m Task
enrichTaskWithCosts t = do cst <- randomRIO (10, 20)
                           awrd <- randomRIO (20, 40)
                           return t { cost = Just cst, reward = Just awrd}

-- На каждый по каналу, все каналы в одном коннекшене. Все действия в цепочки Клейсли выполняются в рамках одной бд транзакции
setupConsumers :: (MonadFail m, ConsumerConstraints RuntimeConfig m a) => m ()
setupConsumers = do createConsumer (crtqueue . taskhub) (addTask <=< debitUser <=< enrichTaskWithCosts)
                    createConsumer (cltqueue . taskhub) (closeTask <=< creditUser <=< getTask )
                    createConsumer (updqueue . taskhub) (void . debitUser <=< changeAssignee)

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



