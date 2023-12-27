{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Rabbit(RabbitConfig(..), TaskEventHub(..), PaymentEventHub(..), setupRabbit, UserEventHub (..), getConnection) where

import Network.AMQP
import Control.Monad.Trans
import qualified Conferer.Source.PropertiesFile as PF
import Conferer.Config (addSource, emptyConfig)
import Conferer.FromConfig
import Conferer
import GHC.Generics
import Control.Monad.Trans.Resource ( allocate, MonadResource )
import qualified Data.Text as T
import Control.Exception (bracket)

data RabbitConfig = RabbitConfig { connectionString :: String, userhub :: UserEventHub, taskhub :: TaskEventHub, paymenthub :: PaymentEventHub } deriving (Show, Generic)
instance FromConfig RabbitConfig
instance DefaultConfig RabbitConfig where
  configDef = RabbitConfig "" configDef configDef configDef

data UserEventHub = UserEventHub { ttqueue :: String, acqueue :: String, userexchange :: String, key :: String} deriving (Show, Generic) -- TODO: сделать bindings для понимания что как раутить
instance FromConfig UserEventHub
instance DefaultConfig UserEventHub where
  configDef = UserEventHub "" "" "" ""

data PaymentEventHub = PaymentEventHub { paymentexchange :: String, anaqueue :: String, anakey :: String} deriving (Show, Generic)
instance FromConfig PaymentEventHub
instance DefaultConfig PaymentEventHub where
  configDef = PaymentEventHub"" "" ""

data TaskEventHub = TaskEventHub { taskexchange :: String, 
                                   crtqueue :: String, crtkey :: String,
                                   cltqueue :: String, cltkey :: String,
                                   updqueue :: String, updkey :: String } deriving (Show, Generic)
instance FromConfig TaskEventHub
instance DefaultConfig TaskEventHub where
  configDef = TaskEventHub "" "" "" "" "" "" ""                                   


setupRabbit :: MonadResource m => String -> m ()
setupRabbit cfgPath = do
    config <- liftIO $ addSource (PF.fromFilePath cfgPath) emptyConfig
    rabbitCfg :: RabbitConfig <- liftIO $ fetch config
    liftIO $ print rabbitCfg
    liftIO $ bracket (openConnection'' (fromURI $ connectionString rabbitCfg)) ((putStrLn "Rabbit setup done" <*). closeConnection) 
      (\c -> do
        conn <- openChannel c

        let createQBind q e k = do declareQueue conn newQueue { queueName = T.pack q }
                                   bindQueue conn (T.pack q) (T.pack e) (T.pack k)

        let createQEXBind q e k = do declareExchange conn newExchange {exchangeName = T.pack e, exchangeType = "topic"} 
                                     createQBind q e k

        --users
        createQEXBind (ttqueue . userhub $ rabbitCfg) (userexchange . userhub $ rabbitCfg) (key . userhub $ rabbitCfg) 
        createQBind (acqueue . userhub $ rabbitCfg) (userexchange . userhub $ rabbitCfg) (key . userhub $ rabbitCfg) 

        --tasks
        createQEXBind (crtqueue . taskhub $ rabbitCfg) (taskexchange . taskhub $ rabbitCfg) (crtkey . taskhub $ rabbitCfg) 
        createQBind (cltqueue . taskhub $ rabbitCfg) (taskexchange . taskhub $ rabbitCfg) (cltkey . taskhub $ rabbitCfg)
        createQBind (updqueue . taskhub $ rabbitCfg) (taskexchange . taskhub $ rabbitCfg) (updkey . taskhub $ rabbitCfg)

        --payments
        createQEXBind (anaqueue . paymenthub $ rabbitCfg) (paymentexchange . paymenthub $ rabbitCfg) (anakey . paymenthub $ rabbitCfg) 
      )

getConnection :: MonadResource m => String -> m Connection
getConnection cfgPath = do
  config <- liftIO $ addSource (PF.fromFilePath cfgPath) emptyConfig     
  rabbitCfg :: RabbitConfig <- liftIO $ fetch config
  (_, conn) <- allocate (openConnection'' (fromURI $ connectionString rabbitCfg)) closeConnection
  return conn 
  
