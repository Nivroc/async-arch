
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack, Text, unpack )
import           Data.Aeson (decode, FromJSON, toJSON)
import qualified Data.Text.Lazy                                              as LText
import qualified Data.Text.Lazy.Encoding                                     as LText
import           Data.Maybe

import           Data.Int (Int64)
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (toString)
import           Data.ByteString.Lazy.Internal ( ByteString )
import           Data.Time.Clock.POSIX ( getPOSIXTime )
import qualified Data.Map.Strict                                             as M

import           Conferer.Config.Internal ( emptyConfig, addSource )
import           Conferer ( fetch )
import qualified Conferer.Source.PropertiesFile                              as PF

import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Control.Monad.Except ( MonadError(throwError) )
import           Control.Monad.Error.Class (liftEither)
import           Control.Arrow ( Arrow((&&&)) )
import           Control.Exception (try)

import           Database.PostgreSQL.Simple.Types (Query (Query))
import           Web.Scotty.Trans
import           Web.Scotty.Internal.Types( ActionError(ActionError) )
import qualified Web.JWT                                                     as J
import           Network.HTTP.Types(status500, Status)
import           Network.HTTP.Types.Status ( status204, status422, status400 )
import           Validation ( validationToEither )
import           Database.PostgreSQL.Simple
import           Model
import           Lens.Micro ( (?~) )


type DBConstraints m = (MonadError (ActionError LText.Text) m, MonadReader RuntimeConfig m, MonadIO m)

main :: IO ()
main = do config <- addSource (PF.fromFilePath "./configs/auth.properties") emptyConfig
          app <- fetch config :: IO ApplicationConfig
          runResourceT (runReaderT program app)

program :: (MonadIO m, MonadReader ApplicationConfig m, MonadResource m) => m ()
program = do config <- ask
             (_, conn) <- allocate (connectPostgreSQL $ T.encodeUtf8 $ T.pack (dbstring config)) ((*> Prelude.putStrLn "connection closed") . close)
             let runtime = RtConfig config conn
             liftIO $ print config
             scottyT (port config) (`runReaderT` runtime) Main.routes

routes :: (MonadReader RuntimeConfig m, MonadIO m) => ScottyT LText.Text m ()
routes = do defaultHandler $ \str -> status status500 *> json str

            delete "/user/:username" $ do
              user <- param "username"
              code <- deleteUser user
              if code > 0 then json $ show code <> ": user " <> show user <> " deleted from the database"
              else json $ show code <> ": no such user " <> show user <> " in the database"

            post    "/user/register"  $ do
              user <- decodeOrThrow body status400
              newUUID <- toString <$> liftAndCatchIO nextRandom
              dbcode <- addUser $ (uuidL ?~ newUUID) user
              json $ show dbcode <> ": user " <> show ((uuidL ?~ newUUID) user) <> " added to the database"

            post    "/user/roles/:username" $ do
              newRoles <- decodeOrThrow body status400
              login <- param "username"
              dbcode <- updateRoles newRoles login
              json $ show dbcode <> " roles of user " <> show login <> " updated to " <> show newRoles

            post    "/user/auth"  $ do
              bod :: Creds <- decodeOrThrow body status400
              usr <- findUser $ username bod
              if secret usr == password bod then authSuccess bod usr else json $ show ("Incorrect password" :: String)

            get     "/user/all"       $ fetchUsers >>= json . ("users recovered:\n" <>) . show
            get     "/user/:username" $ param "username" >>= findUser >>= json . ("User found: " <>) . show

            let auth = do let err = throwError $ ActionError status400 "Error: no 'authorization' header"
                          tokenText <- LText.toStrict . LText.drop 7 <$> (header "authorization" >>= maybe err pure)
                          let er = throwError $ ActionError status422 "Auth JWT cannot be decoded"
                          token <- maybe er pure (J.decode tokenText)
                          valid <- tokenExists tokenText >>= \e -> flip (&&) (not $ and $ fromOnly <$> e) <$> liftIO (checkClaims token)
                          if valid then pure $ Just tokenText else pure Nothing

            post    "/user/verify" $  do success <- isJust <$> auth
                                         json $ if success then "Authorization: success" :: String else "Authorization failure"

            put     "/user/logout" $  do token <- auth
                                         case token of
                                           Nothing -> json ("Token expired" :: String)
                                           Just txt -> blacklistToken txt *> json ("User logged out" :: String)


checkClaims :: J.JWT J.UnverifiedJWT -> IO Bool
checkClaims t = getPOSIXTime >>= \curr -> let checkIssuer = (== "PapugAuth") . J.stringOrURIToText <$> J.iss (J.claims t)
                                              checkExpiration = (<$> J.exp (J.claims t)) . (<) =<< J.numericDate curr
                                          in case (checkIssuer, checkExpiration) of
                                                  (Just True, Just True) -> pure True
                                                  _ -> pure False

authSuccess :: (MonadReader RuntimeConfig m, MonadIO m) => Creds -> User -> ActionT LText.Text m ()
authSuccess creds usr = let rol = J.ClaimsMap $ M.fromList [(T.pack "roles", toJSON $ roles usr)]
                            sb  = J.stringOrURI . T.pack . Model.sub $ creds
                            uns t = (mempty {J.iss = J.stringOrURI "PapugAuth", J.sub = sb, J.exp = t, J.unregisteredClaims = rol})
                            tok t = J.encodeUnsigned (uns t) mempty
                        in do time <- liftIO getPOSIXTime
                              expire <- asks (tokenexpiration . cfg)
                              json $ Token (tok . J.numericDate $ (fromIntegral expire + time)) expire (roles usr)

decodeOrThrow :: (MonadError (ActionError LText.Text) m, FromJSON a) => m ByteString -> Status -> m a
decodeOrThrow b s = b >>= \bb -> maybe (throwError $ ActionError s ("Error: " <> LText.decodeUtf8 bb <> " cannot be decoded")) pure (decode bb)


-- database algebra ( would be abstracted to a typeclass in a bigger service )

toQuery :: String -> Query
toQuery = Query . T.encodeUtf8 . T.pack

usert :: MonadReader RuntimeConfig m => m String
usert = asks (uncurry ((<>) . (<> ".")) . (schema &&& usertable) . cfg)

simpleDB :: DBConstraints m => (String -> Query) -> (Query -> Connection -> IO a) -> m a
simpleDB q t = usert >>= \table -> join $ asks (absorbError . liftIO . try . t (q table) . dbConnection)
               where absorbError :: (DBConstraints m) => m (Either SqlError a) -> m a
                     absorbError = (either (throwError . ActionError status422 . LText.pack . show) pure =<<)

deleteUser :: DBConstraints m => String -> m Int64
deleteUser uid = simpleDB (\ table -> toQuery $ "delete from " <> table <> " where login = ?" )
                          (\ q c -> execute c q (Only uid))

addUser :: DBConstraints m => User -> m Int64
addUser u = do validUser <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateUser $ u
               simpleDB (\ table -> toQuery $ "insert into " <> table <> "(uuid, login, email, secret, roles) values (?,?,?,?,?)" )
                        (\ q c -> execute c q validUser)

fetchUsers :: DBConstraints m => m [User]
fetchUsers = simpleDB (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table)
                      (flip query_)


findUser :: DBConstraints m => String -> m User
findUser uid = do users <- simpleDB (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table <> " where login = ?" )
                                    (\ q c -> query c q (Only uid))                                 
                  case users of
                    [x] -> pure x
                    _ -> throwError $ ActionError status204 "No users present"

updateRoles :: DBConstraints m => [Role] -> String -> m Int64
updateRoles r l = do validRoles <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateRoles $ r
                     simpleDB (\ table -> toQuery $ "update " <> table <> " set roles = (?) where login = (?)" )
                              (\ q c -> execute c q (validRoles, l))

tokenExists :: DBConstraints m => T.Text -> m [Only Bool]
tokenExists tok = do conf <- asks cfg
                     let table = schema conf <> "." <> blacklist conf :: String
                     simpleDB (\ _ -> toQuery $ "select exists(select token from " <> table <> " where token = (?) )")
                              (\ q c -> query c q (Only $ T.unpack tok))

blacklistToken :: DBConstraints m => T.Text -> m Int64                              
blacklistToken tok = do conf <- asks cfg
                        let table = schema conf <> "." <> blacklist conf :: String
                        simpleDB (\ _ -> toQuery $ "insert into " <> table <> " (token) values (?)")
                                 (\ q c -> execute c q (Only tok))
