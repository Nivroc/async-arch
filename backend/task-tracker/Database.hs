{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Database where

import           Data.Int (Int64)
import           Database.PostgreSQL.Simple
import           Model
import           Common
import           Data.UUID (UUID)
import           Control.Monad.Reader
import Data.Functor (($>))

-- Походы в базу

addTask :: DBConstraints m RuntimeConfig => Task -> m Int64
addTask u = do simpleDB (tasktable . postgres . cfg)
                        (\ table -> toQuery $ "insert into " <> table <> " (uuid, id, name, desc, status, assignee) values (?,?,?,?,?,?)" )
                        (\ q c -> execute c q u)

addUser :: (MonadIO m, MonadReader RuntimeConfig m) => String -> m Int64
addUser u = do rt <- ask
               let table = schematable rt (usertable . postgres . cfg)
               let q = toQuery $ "insert into " <> table <> " (uuid, fullname) values (?, '')"
               liftIO $ execute (dbConnection rt) q (Only u)

closeTask :: DBConstraints m RuntimeConfig => UUID -> m Int64
closeTask u = do simpleDB (tasktable . postgres . cfg)
                          (\ table -> toQuery $ "update " <> table <> " set status = False where uuid = (?)" )
                          (\ q c -> execute c q (Only u))

checkUserExistsTask :: DBConstraints m RuntimeConfig => UUID -> m [Only Bool]
checkUserExistsTask u = do simpleDB (usertable . postgres . cfg)
                                    (\ table -> toQuery $ "select exists(select uuid from " <> table <> " where uuid = (?))" )
                                    (\ q c -> query c q (Only u))

shuffleView :: DBConstraints m RuntimeConfig => String -> m String
shuffleView postfix = do
    rt <- ask
    let utable = schematable rt (usertable . postgres . cfg)
    let ttable = schematable rt (tasktable . postgres . cfg)
    let q = toQuery $ "  DO $$ \n\
                       \ DECLARE users_num integer; \n\
                       \ BEGIN \n\
                       \ users_num := (select count(*) from asyncarch.ttusers); \n\
                       \ create table " <> ttable <> "_" <> postfix <> " as ( \n\
                       \ with randusers as (SELECT row_number() over () as idt, uuid as newas FROM " <> utable <> " ORDER BY RANDOM()),\
                       \ ordtasks as (SELECT floor(random()*users_num)+1 as idu, uuid, id, name, description, open, assignee FROM " <> ttable <>")\
                       \ select uuid, id, name, description, open, newas as assignee FROM ordtasks\
                       \ left join randusers\ 
                       \ on randusers.idt = ordtasks.idu\ 
                       \ where ordtasks.open = true); \n\
                       \ truncate table " <> ttable <> "; \n\
                       \ insert into " <> ttable <> " select uuid, id, name, description, open, assignee from " <> ttable <> "_" <> postfix <> "; \n\
                       \ commit; \n\
                       \ END $$;"
    liftIO $ execute (dbConnection rt) q () $> ttable

fetchTasks :: DBConstraints m RuntimeConfig => UUID -> m [Task]
fetchTasks u = do simpleDB (tasktable . postgres . cfg)
                           (\ table -> toQuery $ "select * from " <> table <> " where assignee = (?)" )
                           (\ q c -> query c q (Only u))

count :: (DBConstraints m RuntimeConfig, MonadFail m) => String -> m (String, Int64)
count tbl = do rt <- ask
               let q = toQuery $ "select count(*) from " <> tbl <> ";"
               [Only i] <- liftIO $ query (dbConnection rt) q ()
               return (tbl, i)                         

dropTempTable :: DBConstraints m RuntimeConfig => String -> m Int64
dropTempTable tbl = do rt <- ask
                       let q = toQuery $ "drop table " <> tbl <> ";"
                       liftIO $ execute (dbConnection rt) q ()