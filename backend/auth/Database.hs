{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database(deleteUser, addUser, fetchUsers, findUser, updateRoles, tokenExists, blacklistToken, insertClient, fetchClient, deleteClient) where

import           Data.Text.Encoding                                          as T ( encodeUtf8 )
import           Data.Text                                                   as T ( pack, Text, unpack )
import qualified Data.Text.Lazy                                              as LText
import           Data.Int (Int64)
import           Control.Monad.Reader
import           Control.Monad.Except ( MonadError(throwError) )
import           Control.Monad.Error.Class (liftEither)
import           Control.Arrow ( Arrow((&&&)) )
import           Control.Exception (try)

import           Database.PostgreSQL.Simple.Types (Query (Query))
import           Web.Scotty.Internal.Types( ActionError(ActionError) )
import           Network.HTTP.Types.Status ( status204, status422)
import           Validation ( validationToEither )
import           Database.PostgreSQL.Simple
import           Model            


type DBConstraints m = (MonadError (ActionError LText.Text) m, MonadReader RuntimeConfig m, MonadIO m)

toQuery :: String -> Query
toQuery = Query . T.encodeUtf8 . T.pack

anyt :: MonadReader RuntimeConfig m => (ApplicationConfig -> String) -> m String
anyt t = asks (uncurry ((<>) . (<> ".")) . (schema &&& t) . cfg)

simpleDB :: DBConstraints m => (ApplicationConfig -> String) -> (String -> Query) -> (Query -> Connection -> IO a) -> m a
simpleDB c q t = anyt c >>= \table -> join $ asks (absorbError . liftIO . try . t (q table) . dbConnection)
                 where absorbError :: (DBConstraints m) => m (Either SqlError a) -> m a
                       absorbError = (either (throwError . ActionError status422 . LText.pack . show) pure =<<)

deleteUser :: DBConstraints m => String -> m Int64
deleteUser uid = simpleDB usertable (\ table -> toQuery $ "delete from " <> table <> " where login = ?" )
                                    (\ q c -> execute c q (Only uid))

addUser :: DBConstraints m => User -> m Int64
addUser u = do validUser <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateUser $ u
               simpleDB usertable (\ table -> toQuery $ "insert into " <> table <> "(uuid, login, email, secret, roles) values (?,?,?,?,?)" )
                                  (\ q c -> execute c q validUser)

fetchUsers :: DBConstraints m => m [User]
fetchUsers = simpleDB usertable (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table)
                                (flip query_)

findUser :: DBConstraints m => String -> m User
findUser uid = do users <- simpleDB usertable (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table <> " where login = ?" )
                                              (\ q c -> query c q (Only uid))                                 
                  case users of
                    [x] -> pure x
                    _ -> throwError $ ActionError status204 "No users present"

updateRoles :: DBConstraints m => [Role] -> String -> m Int64
updateRoles r l = do validRoles <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateRoles $ r
                     simpleDB usertable (\ table -> toQuery $ "update " <> table <> " set roles = (?) where login = (?)" )
                                        (\ q c -> execute c q (validRoles, l))

tokenExists :: DBConstraints m => T.Text -> m [Only Bool]
tokenExists tok = simpleDB blacklist (\ table -> toQuery $ "select exists(select token from " <> table <> " where token = (?) )")
                                     (\ q c -> query c q (Only $ T.unpack tok))

blacklistToken :: DBConstraints m => T.Text -> m Int64                              
blacklistToken tok = simpleDB blacklist (\ table -> toQuery $ "insert into " <> table <> " (token) values (?)")
                                        (\ q c -> execute c q (Only tok))

insertClient :: DBConstraints m => ClientApp -> m Int64
insertClient cli = simpleDB clienttable (\ table -> toQuery $ "insert into " <> table <> " (clientid, clientsecret) values (?,?)")
                                        (\ q c -> execute c q cli)

fetchClient :: DBConstraints m => T.Text -> m [ClientApp]
fetchClient cli = simpleDB clienttable (\ table -> toQuery $ "select clientid, clientsecret from " <> table <> " where clientid = (?)")
                                       (\ q c -> query c q (Only cli))

deleteClient :: DBConstraints m => T.Text -> m Int64
deleteClient cli = simpleDB clienttable (\ table -> toQuery $ "delete from " <> table <> " where clientid = (?)")
                                        (\ q c -> execute c q (Only $ T.unpack cli))