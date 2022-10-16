{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main(main) where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack )
import           Data.Aeson (toJSON)
import qualified Data.Text.Lazy                                              as TL
import           Data.Maybe ( isJust, listToMaybe )
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (toString)
import           Data.Time.Clock.POSIX ( getPOSIXTime )
import qualified Data.Map.Strict                                             as M

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF
import           Control.Monad.Trans.Resource( allocate, runResourceT, MonadResource )
import           Control.Monad.Reader
import           Control.Monad.Except ( MonadError(throwError) )
import           Control.Arrow ((&&&))
import           Web.Scotty.Trans
import           Web.Scotty.Internal.Types( ActionError(ActionError) )
import qualified Web.JWT                                                     as J
import           Network.HTTP.Types(status500)
import           Network.HTTP.Types.Status ( status422, status400 )
import           Network.AMQP (publishMsg, Message (..), newMsg)
import           Database.PostgreSQL.Simple ( close, connectPostgreSQL, Only(fromOnly) )
import           Lens.Micro ( (?~) )
import           Model
import           Database
import           GHC.Base ((<|>))
import           Common
import           Rabbit (setupRabbit, UserEventHub (..))
import qualified Data.Text.Lazy.Encoding as TL



main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/auth.properties") emptyConfig
          mainApp <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT programP mainApp)

programP :: (MonadIO m, MonadReader ApplicationConfig m, MonadResource m) => m ()
programP = do config <- ask
              (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring config)) ((*> Prelude.putStrLn "connection closed") . close)
              chan <- setupRabbit "./configs/rabbitmq.properties"
              let runtime = RtConfig config conn chan
              liftIO $ print config
              scottyT (port config) (`runReaderT` runtime) Main.routes

routes :: (MonadReader RuntimeConfig m, MonadIO m, MonadPlus m) => ScottyT TL.Text m ()
routes = do midware
            defaultHandler $ \str -> status status500 *> json str
  
            delete "/user/:username" $ do
              user <- param "username"
              code <- deleteUser user
              if code > 0 then json $ show code <> ": user " <> show user <> " deleted from the database"
              else json $ show code <> ": no such user " <> show user <> " in the database"

            post    "/user/register"  $ do
              user <- decodeOrThrow body status400
              newUUID <- toString <$> liftAndCatchIO nextRandom
              let enrichedUser = (uuidL ?~ newUUID) user
              dbcode <- addUser enrichedUser
              sendUserCreated enrichedUser
              json $ show dbcode <> ": user " <> show enrichedUser <> " added to the database"

            post    "/user/roles/:username" $ do
              newRoles <- decodeOrThrow body status400
              ulogin <- param "username"
              dbcode <- updateRoles newRoles ulogin
              json $ show dbcode <> " roles of user " <> show ulogin <> " updated to " <> show newRoles

            let validateApp bod = do mcli <- listToMaybe <$> fetchClient bod
                                     maybe (throwError $ ActionError status422 "No such client app registered") pure mcli  

            post    "/user/auth"  $ do
              bod :: Creds <- decodeOrThrow body status400
              usr <- findUser $ username bod
              validateApp . T.pack . clientid . app $ bod
              if secret usr == password bod
              then (authSigned bod usr >>= tokenToHeaders) <|> (Web.Scotty.Trans.redirect . TL.fromStrict . Model.redirect $ bod)
              else json $ show ("Incorrect password" :: String)

            get     "/user/all"       $ fetchUsers >>= json . ("users recovered:\n" <>) . show
            get     "/user/:username" $ param "username" >>= findUser >>= json . ("User found: " <>) . show

            let auth clientId = do let err = throwError $ ActionError status400 "Error: no 'authorization' header"
                                   tokenText <- TL.toStrict . TL.drop 7 <$> (header "authorization" >>= maybe err pure)
                                   let er = throwError $ ActionError status422 "Auth JWT cannot be decoded"
                                   cli <- validateApp $ T.pack clientId
                                   tok <- maybe er pure (J.decodeAndVerifySignature (J.toVerify . J.hmacSecret $ clientSecret cli) tokenText)
                                   valid <- tokenExists tokenText >>= \e -> flip (&&) (not $ and $ fromOnly <$> e) <$> liftIO (checkClaims tok)
                                   if valid then pure $ Just tokenText else pure Nothing

            post    "/user/verify/:client" $  do clientId <- param "client"
                                                 success <- isJust <$> auth clientId
                                                 json $ if success then "Authorization: success" :: String else "Authorization failure"

            put     "/user/logout/:client" $  do clientId <- param "client"
                                                 tok <- auth clientId
                                                 case tok of
                                                    Nothing -> json ("Token expired" :: String)
                                                    Just txt -> blacklistToken txt *> json ("User logged out" :: String)

            put     "/app/register/:clientid" $ do secr <- T.pack . toString <$> liftAndCatchIO nextRandom
                                                   param "clientid" >>= insertClient . flip ClientApp secr
                                                   text $ TL.fromStrict secr

sendUserCreated :: (MonadReader RuntimeConfig m, MonadIO m) => User -> ActionT TL.Text m (Maybe Int)
sendUserCreated usr = do (exc, routingKey) <- asks ((T.pack . exchange &&& T.pack . key) . userhub . cfg) 
                         chan <- asks rmqchan 
                         let e = throwError $ ActionError status400 "This is an impossible state in our application. If you are seeing this please contact noone."
                         maybe e (\x -> liftIO $ publishMsg chan exc routingKey (newMsg {msgBody = TL.encodeUtf8 . TL.pack . show $ x})) (uuid usr)

tokenToHeaders :: (MonadPlus m) => Token -> ActionT TL.Text m ()
tokenToHeaders tok = setHeader "authorization" (TL.fromStrict $ token tok) <|>
                     setHeader "expires" (TL.pack $ show $ expiresIn tok) <|>
                     setHeader "roles" (TL.pack $ show $ assignedRoles tok)

checkClaims :: J.JWT J.VerifiedJWT -> IO Bool
checkClaims t = getPOSIXTime >>= \curr -> let checkIssuer = (== "PapugAuth") . J.stringOrURIToText <$> J.iss (J.claims t)
                                              checkExpiration = (<$> J.exp (J.claims t)) . (<) =<< J.numericDate curr
                                          in case (checkIssuer, checkExpiration) of
                                                  (Just True, Just True) -> pure True
                                                  _ -> pure False

authSigned :: (MonadReader RuntimeConfig m, MonadIO m) => Creds -> User -> m Token
authSigned creds usr = let rol = J.ClaimsMap $ M.fromList [(T.pack "roles", toJSON $ roles usr)]
                           uns t = (mempty {J.iss = J.stringOrURI "PapugAuth", J.exp = t, J.unregisteredClaims = rol})
                           tok t = J.encodeSigned (J.hmacSecret $ clientSecret $ app creds) mempty (uns t)
                       in do time <- liftIO getPOSIXTime
                             expire <- asks (tokenexpiration . cfg)
                             return $ Token (tok . J.numericDate $ (fromIntegral expire + time)) expire (roles usr)
