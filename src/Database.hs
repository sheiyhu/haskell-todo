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

import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Types (Todo (..))

-- | Connect to the database using environment variables
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

-- | Auto-close connection safely
withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket connectDB close

-- | Insert & return the full record (completed defaults to False)
insertTodo :: Text -> IO Todo
insertTodo title =
  withConnection $ \conn -> do
    now <- getCurrentTime
    [Only newId] <-
      query
        conn
        "INSERT INTO todos (title, created_at) VALUES (?, ?) RETURNING id"
        (title, now)
    return $ Todo newId title now Nothing False

-- | Fetch all todos
fetchTodos :: IO [Todo]
fetchTodos =
  withConnection $ \conn ->
    query_
      conn
      "SELECT id, title, created_at, completed_at, completed \
      \FROM todos ORDER BY id"

-- | Fetch single todo by ID
fetchTodoById :: Int -> IO (Maybe Todo)
fetchTodoById tid =
  withConnection $ \conn -> do
    results <-
      query
        conn
        "SELECT id, title, created_at, completed_at, completed \
        \FROM todos WHERE id = ?"
        (Only tid)
    return $ case results of
      [t] -> Just t
      _ -> Nothing

-- | Update todo's title
updateTodoName :: (Int, Text) -> IO (Maybe Todo)
updateTodoName (tid, title) =
  withConnection $ \conn -> do
    _ <- execute conn "UPDATE todos SET title = ? WHERE id = ?" (title, tid)
    results <-
      query
        conn
        "SELECT id, title, created_at, completed_at, completed \
        \FROM todos WHERE id = ?"
        (Only tid)
    return $ case results of
      [t] -> Just t
      _ -> Nothing

-- | Mark as complete
completeTodo :: Int -> IO (Maybe Todo)
completeTodo tid =
  withConnection $ \conn -> do
    now <- getCurrentTime
    _ <-
      execute
        conn
        "UPDATE todos SET completed_at = ?, completed = TRUE WHERE id = ?"
        (Just now, tid)
    results <-
      query
        conn
        "SELECT id, title, created_at, completed_at, completed \
        \FROM todos WHERE id = ?"
        (Only tid)
    return $ case results of
      [t] -> Just t
      _ -> Nothing

-- | Delete by ID
deleteTodo :: Int -> IO Bool
deleteTodo tid =
  withConnection $ \conn -> do
    n <- execute conn "DELETE FROM todos WHERE id = ?" (Only tid)
    return (n == 1)
