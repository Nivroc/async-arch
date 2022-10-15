{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Database(deleteUser, addUser, fetchUsers, findUser, updateRoles, tokenExists, blacklistToken, insertClient, fetchClient, deleteClient) where
import           Data.Text                                                   as T ( Text, unpack )
import qualified Data.Text.Lazy                                              as LText
import           Data.Int (Int64)
import           Control.Monad.Except ( MonadError(throwError) )
import           Control.Monad.Error.Class (liftEither)
import           Web.Scotty.Internal.Types( ActionError(ActionError) )
import           Network.HTTP.Types.Status ( status204, status422)
import           Validation ( validationToEither )
import           Database.PostgreSQL.Simple
import           Model    
import           Common        

ut :: RuntimeConfig -> String
ut = usertable . cfg

deleteUser :: DBConstraints m RuntimeConfig => String -> m Int64
deleteUser uid = simpleDB ut (\ table -> toQuery $ "delete from " <> table <> " where login = ?" )
                             (\ q c -> execute c q (Only uid))

addUser :: DBConstraints m RuntimeConfig => User -> m Int64
addUser u = do validUser <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateUser $ u
               simpleDB ut (\ table -> toQuery $ "insert into " <> table <> "(uuid, login, email, secret, roles) values (?,?,?,?,?)" )
                           (\ q c -> execute c q validUser)

fetchUsers :: DBConstraints m RuntimeConfig => m [User]
fetchUsers = simpleDB ut (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table)
                         (flip query_)

findUser :: DBConstraints m RuntimeConfig => String -> m User
findUser uid = do users <- simpleDB ut (\ table -> toQuery $ "select uuid, login, email, secret, roles from " <> table <> " where login = ?" )
                                       (\ q c -> query c q (Only uid))                                 
                  case users of
                    [x] -> pure x
                    _ -> throwError $ ActionError status204 "No users present"

updateRoles :: DBConstraints m RuntimeConfig => [Role] -> String -> m Int64
updateRoles r l = do validRoles <- liftEither . either (throwError . ActionError status422 . LText.pack . show) pure . validationToEither . validateRoles $ r
                     simpleDB ut (\ table -> toQuery $ "update " <> table <> " set roles = (?) where login = (?)" )
                                 (\ q c -> execute c q (validRoles, l))

tokenExists :: DBConstraints m RuntimeConfig => T.Text -> m [Only Bool]
tokenExists tok = simpleDB (blacklist . cfg) 
                           (\ table -> toQuery $ "select exists(select token from " <> table <> " where token = (?) )")
                           (\ q c -> query c q (Only $ T.unpack tok))

blacklistToken :: DBConstraints m RuntimeConfig => T.Text -> m Int64                              
blacklistToken tok = simpleDB (blacklist . cfg) 
                              (\ table -> toQuery $ "insert into " <> table <> " (token) values (?)")
                              (\ q c -> execute c q (Only tok))

insertClient :: DBConstraints m RuntimeConfig => ClientApp -> m Int64
insertClient cli = simpleDB (clienttable . cfg) 
                            (\ table -> toQuery $ "insert into " <> table <> " (clientid, clientsecret) values (?,?)")
                            (\ q c -> execute c q cli)

fetchClient :: DBConstraints m RuntimeConfig => T.Text -> m [ClientApp]
fetchClient cli = simpleDB (clienttable . cfg) (\ table -> toQuery $ "select clientid, clientsecret from " <> table <> " where clientid = (?)")
                                       (\ q c -> query c q (Only cli))

deleteClient :: DBConstraints m RuntimeConfig => T.Text -> m Int64
deleteClient cli = simpleDB (clienttable . cfg) 
                            (\ table -> toQuery $ "delete from " <> table <> " where clientid = (?)")
                            (\ q c -> execute c q (Only $ T.unpack cli))