{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import           Data.Text.Encoding                                          as T ( encodeUtf8)
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Applicative (Applicative(..))
import           Web.Scotty.Trans
import           Network.HTTP.Types(status500)
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL )
import           Model
import           Common
import           Rabbit (setupRabbit, getConnection)
import           Control.Monad.Trans.Control

main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/accounting.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program mainApp)


program :: (MonadReader ApplicationConfig m, MonadResource m) => m ()
program = do config <- ask
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring $ postgres config)) ((*> Prelude.putStrLn "connection closed") . close)
             let rabbitCfgPath = "./configs/rabbitmq.properties"
             setupRabbit rabbitCfgPath
             (pubConn, consumeConn) <- liftA2 (,) (getConnection rabbitCfgPath) (getConnection rabbitCfgPath)
             let runtime = RtConfig config conn pubConn consumeConn
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

--обработка сообщений
--1)зареган/удален юзер
--2)открыта/закрыта таска
