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

-- Походы в базу

addTask :: DBConstraints m RuntimeConfig => Task -> m Int64
addTask u = do simpleDB (tasktable . cfg) 
                        (\ table -> toQuery $ "insert into " <> table <> " (uuid, id, name, desc, status, assignee) values (?,?,?,?,?,?)" )
                        (\ q c -> execute c q u)

closeTask :: DBConstraints m RuntimeConfig => UUID -> m Int64
closeTask u = do simpleDB (tasktable . cfg) 
                          (\ table -> toQuery $ "update " <> table <> " set status = False where uuid = (?)" )
                          (\ q c -> execute c q (Only u)) 
                          
checkUserExistsTask :: DBConstraints m RuntimeConfig => UUID -> m [Only Bool]
checkUserExistsTask u = do simpleDB (usertable . cfg) 
                                    (\ table -> toQuery $ "select exists(select uuid from " <> table <> " where uuid = (?))" )
                                    (\ q c -> query c q (Only u))        

-- TODO
reassignTasks = undefined   

fetchTasks :: DBConstraints m RuntimeConfig => UUID -> m [Task]
fetchTasks u = do simpleDB (tasktable . cfg) 
                           (\ table -> toQuery $ "select * from " <> table <> " where assignee = (?)" )
                           (\ q c -> query c q (Only u))   