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


addTask :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m ()
addTask t = do rt <- ask
               let table = schematable rt (taskcost . postgres . cfg)
               let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, description, open, assignee) values (?,?,?,?,?,?)"
               void $ liftIO $ execute (dbConnection rt) q t

closeTask :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m ()
closeTask t = do rt <- ask
                 let table = schematable rt (taskcost . postgres . cfg)
                 let q = toQuery $ "update " <> table <> " set open = False where uuid = (?)"
                 void $ liftIO $ execute (dbConnection rt) q (Only $ uuid t)               

creditUser :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m Task
creditUser t = do rt <- ask
                  let table = schematable rt (credit . postgres . cfg)
                  newUUID <- liftIO nextRandom
                  ts <- liftIO getCurrentTime
                  let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,?,?,?,?,?,?)"
                  liftIO $ execute (dbConnection rt) q (newUUID, title t, jira_id t, assignee t, description t, cost t, ts)
                  return t

debitUser :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m Task
debitUser t = do rt <- ask
                 let table = schematable rt (credit . postgres . cfg)
                 newUUID <- liftIO nextRandom
                 ts <- liftIO getCurrentTime
                 let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,?,?,?,?,?,?)"
                 liftIO $ execute (dbConnection rt) q (newUUID, title t, jira_id t, assignee t, description t, reward t, ts)
                 return t

changeAssignee :: (MonadIO m, MonadReader RuntimeConfig m) => Task -> m Task
changeAssignee t = do rt <- ask
                      let table = schematable rt (taskcost . postgres . cfg)
                      let q = toQuery $ "update " <> table <> " set assignee = (?) where uuid = (?) "
                      void $ liftIO $ execute (dbConnection rt) q (assignee t, uuid t)
                      return t     
