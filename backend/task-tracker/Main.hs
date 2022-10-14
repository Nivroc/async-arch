module Main where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack )
import qualified Data.Text.Lazy                                              as TL

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource( allocate, runResourceT, MonadResource )
import           Control.Monad.Reader
import           Web.Scotty.Trans
import           Network.HTTP.Types(status500)
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL )
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

routes :: (MonadReader RuntimeConfig m, MonadIO m, MonadPlus m) => ScottyT TL.Text m ()
routes = do midware 
            defaultHandler $ \str -> status status500 *> json str  
            
            get "/assigned/:userid" undefined --TODO: проверяем роль Worker и отдаем все таски по айдишнику, отсылаем кафка эвент

            put "/shuffle" undefined --TODO: проверяем роль Admin и шафлим все таски, отсылаем батч эвентов

            put "/close/:taskid" undefined -- TODO: закрываем таску, отсылаем кафка эвент

            post "/create/:task" undefined -- TODO: генерируем UID, засовываем таску в базу, отсылаем кафка эвент


--TODO: Проверяем токен у юзера            
signedIn = undefined 

--TODO: проверяем наличие нужной роли
hasRole = undefined


--TODO: get kafka event and add user
registerUser = undefined 

--TODO: get kafka event and remove user
removeUser = undefined 
 