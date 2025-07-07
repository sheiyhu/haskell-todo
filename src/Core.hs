module Core
  ( createTodo
  , listTodos
  , getTodo
  , markDone
  , removeTodo
  , updateTodo
  ) where

import Types (Todo)
import Database
import Data.Text (Text)

createTodo :: Text -> IO Todo
createTodo title = do
  conn <- connectDB
  insertTodo conn title

listTodos :: IO [Todo]
listTodos = do
  conn <- connectDB
  fetchTodos conn

getTodo :: Int -> IO (Maybe Todo)
getTodo tid = do
  conn <- connectDB
  fetchTodoById conn tid

updateTodo :: (Int, Text) -> IO (Maybe Todo)
updateTodo (tid, title) = do
  conn <- connectDB
  updateTodoName conn (tid, title)

markDone :: Int -> IO (Maybe Todo)
markDone tid = do
  conn <- connectDB
  completeTodo conn tid

removeTodo :: Int -> IO Bool
removeTodo tid = do
  conn <- connectDB
  deleteTodo conn tid