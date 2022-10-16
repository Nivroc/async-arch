{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Rabbit(setupRabbit, UserEventHub (..)) where
    
import Network.AMQP
import Control.Monad.Trans
import qualified Conferer.Source.PropertiesFile as PF
import Conferer.Config (addSource, emptyConfig)
import Conferer.FromConfig
import Conferer
import GHC.Generics
import Control.Monad.Trans.Resource ( allocate, MonadResource )
import qualified Data.Text as T

data RabbitConfig = RabbitConfig { connectionString :: String, userhub :: UserEventHub } deriving (Show, Generic)
data UserEventHub = UserEventHub { queue :: String, exchange :: String, key :: String} deriving (Show, Generic) -- TODO: сделать bindings для понимания что как раутить
instance FromConfig UserEventHub
instance DefaultConfig UserEventHub where
  configDef = UserEventHub "" "" ""
instance FromConfig RabbitConfig
instance DefaultConfig RabbitConfig where
  configDef = RabbitConfig "" configDef

setupRabbit :: MonadResource m => String -> m Channel
setupRabbit cfgPath = do    
    config <- liftIO $ addSource (PF.fromFilePath cfgPath) emptyConfig
    rabbitCfg :: RabbitConfig <- liftIO $ fetch config 
    (_, conn) <- allocate (openConnection'' $ fromURI $ connectionString rabbitCfg) closeConnection
    chan <- liftIO $ openChannel conn
    let ex = T.pack $ exchange . userhub $ rabbitCfg
    liftIO $ do declareQueue chan newQueue { queueName = T.pack $ queue . userhub $ rabbitCfg }
                declareExchange chan newExchange {exchangeName = ex, exchangeType = "topic"}
                bindQueue chan (T.pack $ queue . userhub $ rabbitCfg) ex (T.pack . key . userhub $ rabbitCfg)
    return chan
