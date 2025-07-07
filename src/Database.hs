{-# LANGUAGE OverloadedStrings #-}

module Database
  ( connectDB,
    insertTodo,
    fetchTodos,
    fetchTodoById,
    completeTodo,
    deleteTodo,
    updateTodoName,
  )
where

import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Types (Todo (..))

-- | Single, plain connection (no pooling):
connectDB :: IO Connection
connectDB = do
  host <- getEnv "DB_HOST"
  db <- getEnv "DB_NAME"
  user <- getEnv "DB_USER"
  pass <- getEnv "DB_PASS"
  port <- getEnv "DB_PORT"
  let connStr =
        pack $
          "host="
            <> host
            <> " port="
            <> port
            <> " dbname="
            <> db
            <> " user="
            <> user
            <> " password="
            <> pass
  connectPostgreSQL connStr

-- | Insert & return the full record (completed defaults to False):
insertTodo :: Connection -> Text -> IO Todo
insertTodo conn title = do
  now <- getCurrentTime
  [Only newId] <-
    query
      conn
      "INSERT INTO todos (title, created_at) VALUES (?, ?) RETURNING id"
      (title, now)
  -- completed_at is NULL, completed is default False
  return $ Todo newId title now Nothing False

-- | Fetch all todos, including our new flag:
fetchTodos :: Connection -> IO [Todo]
fetchTodos conn =
  query_
    conn
    "SELECT id, title, created_at, completed_at, completed \
    \FROM todos ORDER BY id"

-- | Lookup one:
fetchTodoById :: Connection -> Int -> IO (Maybe Todo)
fetchTodoById conn tid = do
  results <-
    query
      conn
      "SELECT id, title, created_at, completed_at, completed \
      \FROM todos WHERE id = ?"
      (Only tid)
  return $ case results of
    [t] -> Just t
    _ -> Nothing

-- | Update a todo: name
updateTodoName :: Connection -> (Int, Text) -> IO (Maybe Todo)
updateTodoName conn (tid, title) = do
  _ <-
    execute
      conn
      "UPDATE todos SET title = ? WHERE id = ?"
      (Just title, tid)
  fetchTodoById conn tid

-- | Mark as complete: set both completed_at and completed flag
completeTodo :: Connection -> Int -> IO (Maybe Todo)
completeTodo conn tid = do
  now <- getCurrentTime
  _ <-
    execute
      conn
      "UPDATE todos SET completed_at = ?, completed = TRUE WHERE id = ?"
      (Just now, tid)
  fetchTodoById conn tid

-- | Delete a todo:
deleteTodo :: Connection -> Int -> IO Bool
deleteTodo conn tid = do
  n <- execute conn "DELETE FROM todos WHERE id = ?" (Only tid)
  return (n == 1)