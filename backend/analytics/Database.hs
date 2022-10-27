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


addEntry :: (MonadIO m, MonadReader RuntimeConfig m) => (PostgresSettings -> String) -> AuditLogEntry -> m ()
addEntry tbl u = do rt <- ask
                    let table = schematable rt (tbl . postgres . cfg)
                    let q = toQuery $ "insert into " <> table <> " (uuid, title, jira_id, userid, description, amount, ts) values (?,?,?,?,?,?,?)"
                    void . liftIO $ execute (dbConnection rt) q u

allAuditLog :: (MonadIO m, MonadReader RuntimeConfig m) => m [AuditLogEntry]
allAuditLog = do rt <- ask
                 let table = schematable rt (tasks . postgres . cfg)
                 let q = toQuery $ "select uuid, title, jira_id, userid, description, amount, ts from " <> table <> " order by ts desc"
                 liftIO $ query (dbConnection rt) q ()
