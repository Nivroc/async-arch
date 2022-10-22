{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Database where

import Database.PostgreSQL.Simple
import Model
import Common
import Control.Monad ( void )
import Control.Monad.Reader ( MonadReader (ask) )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Lens.Micro ((^.))

addUser :: (MonadIO m, MonadReader RuntimeConfig m) => User -> m ()
addUser u = do rt <- ask
               let table = schematable rt (acusers . postgres . cfg)
               let q = toQuery $ "insert into " <> table <> " (uuid, roles, fullname, email) values (?, ?, ?, ?)"
               void $ liftIO $ execute (dbConnection rt) q u

checkUserExists :: (MonadIO m, MonadReader RuntimeConfig m, MonadFail m) => UUID -> m UUID
checkUserExists u = do rt <- ask
                       let table = schematable rt (acusers . postgres . cfg)
                       let q = toQuery $ "select exists(select uuid from " <> table <> " where uuid = (?) and ('Worker' = ANY (roles)))"
                       [Only True] <- liftIO $ query (dbConnection rt) q (Only u)
                       return u

addTask :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m ()
addTask t = do rt <- ask
               let table = schematable rt (taskcost . postgres . cfg)
               let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, cost, reward, description, open, assignee) values (?,?,?,?,?,?,?,?)"
               void $ liftIO $ execute (dbConnection rt) q t

closeTask :: (MonadIO m, MonadReader RuntimeConfig m) => UUID -> m ()
closeTask t = do rt <- ask
                 let table = schematable rt (taskcost . postgres . cfg)
                 let q = toQuery $ "update " <> table <> " set open = False where uuid = (?)"
                 void $ liftIO $ execute (dbConnection rt) q (Only t)

getTask :: (MonadIO m, MonadReader RuntimeConfig m, MonadFail m) => UUID -> m Task
getTask tid = do rt <- ask
                 let table = schematable rt (taskcost . postgres . cfg)
                 let q = toQuery $ "select uuid, title, jira_id, cost, reward, description, open, assignee from " <> table <> " where uuid = (?)"
                 [task] <- liftIO $ query (dbConnection rt) q (Only tid)
                 return task

creditUser :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m UUID
creditUser t = do rt <- ask
                  let table = schematable rt (credit . postgres . cfg)
                  newUUID <- liftIO nextRandom
                  ts <- liftIO getCurrentTime
                  let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,?,?,?,?,?,?)"
                  liftIO $ execute (dbConnection rt) q (newUUID, title t, jira_id t, assignee t, description t, cost t, ts)
                  return $ t ^. uuidTask

debitUser :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m Task
debitUser t = do rt <- ask
                 let table = schematable rt (credit . postgres . cfg)
                 newUUID <- liftIO nextRandom
                 ts <- liftIO getCurrentTime
                 let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,?,?,?,?,?,?)"
                 liftIO $ execute (dbConnection rt) q (newUUID, title t, jira_id t, assignee t, description t, reward t, ts)
                 return t

addEntry :: (MonadIO m, MonadReader RuntimeConfig m) => (PostgresSettings -> String) -> String -> UUID -> Int -> m (UUID, Int)
addEntry tbl d u amount = do rt <- ask
                             let table = schematable rt (tbl . postgres . cfg)
                             newUUID <- liftIO nextRandom
                             ts <- liftIO getCurrentTime
                             let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,'" <> d <> "','',?,'',?,?)"
                             liftIO $ execute (dbConnection rt) q (newUUID, u, amount, ts)
                             return (u, amount)

-- на случай важных переговоров (\time@(UTCTime (ModifiedJulianDay d) t) -> time {utctDay = ModifiedJulianDay (d + 1), utctDayTime = 1 }) 

changeAssignee :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m Task
changeAssignee t = do rt <- ask
                      let table = schematable rt (taskcost . postgres . cfg)
                      let q = toQuery $ "update " <> table <> " set assignee = (?) where uuid = (?) "
                      void $ liftIO $ execute (dbConnection rt) q (assignee t, t ^. uuidTask)
                      return t

workerAuditLog :: (MonadIO m, MonadReader RuntimeConfig m) => UUID -> m [AuditLogEntry]
workerAuditLog u = do rt <- ask
                      let table = schematable rt (audit . postgres . cfg)
                      let q = toQuery $ "select uuid, title, jira_id, userid, description, amount, ts from " <> table <> " where userid = (?) order by ts desc"
                      liftIO $ query (dbConnection rt) q (Only u)

allAuditLog :: (MonadIO m, MonadReader RuntimeConfig m) => m [AuditLogEntry]
allAuditLog = do rt <- ask
                 let table = schematable rt (audit . postgres . cfg)
                 let q = toQuery $ "select uuid, title, jira_id, userid, description, amount, ts from " <> table <> " order by ts desc"
                 liftIO $ query (dbConnection rt) q ()
