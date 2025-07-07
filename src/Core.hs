module Core
  ( createTodo,
    listTodos,
    getTodo,
    markDone,
    removeTodo,
    updateTodo,
    ValidationError(..)
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database
import Types (Todo)

-- | Custom exception for validation errors
newtype ValidationError
  = ValidationError String
  deriving (Show, Typeable)

instance Exception ValidationError

-- | Validate title is not empty or just whitespace
validateTitle :: Text -> IO ()
validateTitle title =
  when (T.null $ T.strip title) $
    throwIO $ ValidationError "VALIDATION: Title cannot be empty"

-- | Create a new todo
createTodo :: Text -> IO Todo
createTodo title = do
  validateTitle title
  insertTodo title

-- | List all todos
listTodos :: IO [Todo]
listTodos = fetchTodos

-- | Fetch one todo
getTodo :: Int -> IO (Maybe Todo)
getTodo = fetchTodoById

-- | Update todo title
updateTodo :: (Int, Text) -> IO (Maybe Todo)
updateTodo (tid, title) = do
  validateTitle title
  updateTodoName (tid, title)

-- | Mark todo as complete
markDone :: Int -> IO (Maybe Todo)
markDone = completeTodo

-- | Delete a todo
removeTodo :: Int -> IO Bool
removeTodo = deleteTodo