{-# LANGUAGE OverloadedStrings #-}

module HTMXRoutes (addHTMXRoutes) where

import Control.Exception (try)
import Core (ValidationError (..))
import Core qualified
import Data.Text()
import HTMXViews qualified as Views
import Network.HTTP.Types.Status
import Types (Todo (..))
import Web.Scotty

addHTMXRoutes :: ScottyM ()
addHTMXRoutes = do
  -- Main page
  get "/app" $ do
    todos <- liftIO Core.listTodos
    html $ Views.todoPage todos

  -- Get todos list (HTMX endpoint)
  get "/htmx/todos" $ do
    todos <- liftIO Core.listTodos
    html $ Views.todoList todos

  -- Create new todo (HTMX endpoint)
  post "/htmx/todos" $ do
    titleParam <- formParam "title"
    result <- liftIO $ try (Core.createTodo titleParam)
    case result of
      Left (ValidationError msg) -> do
        status badRequest400
        html $ Views.errorMessage msg
      Right _ -> do
        todos <- liftIO Core.listTodos
        html $ Views.todoList todos

  -- Toggle todo completion (HTMX endpoint)
  post "/htmx/todos/:id/toggle" $ do
    tid <- pathParam "id"
    mtodo <- liftIO $ Core.getTodo tid
    case mtodo of
      Nothing -> do
        status notFound404
        html $ Views.errorMessage "Todo not found"
      Just todo -> do
        if todoCompleted todo
          then do
            -- TODO: Add unmarkDone function to Core module
            html $ Views.errorMessage "Cannot uncomplete todos yet"
          else do
            _ <- liftIO $ Core.markDone tid
            todos <- liftIO Core.listTodos
            html $ Views.todoList todos

  -- Delete todo (HTMX endpoint)
  delete "/htmx/todos/:id" $ do
    tid <- pathParam "id"
    ok <- liftIO $ Core.removeTodo tid
    if ok
      then do
        todos <- liftIO Core.listTodos
        html $ Views.todoList todos
      else do
        status notFound404
        html $ Views.errorMessage "Todo not found"

  -- Edit todo form (HTMX endpoint)
  get "/htmx/todos/:id/edit" $ do
    tid <- pathParam "id"
    mtodo <- liftIO $ Core.getTodo tid
    case mtodo of
      Nothing -> do
        status notFound404
        html $ Views.errorMessage "Todo not found"
      Just todo ->
        html $ Views.editTodoForm todo

  -- Update todo (HTMX endpoint)
  put "/htmx/todos/:id" $ do
    tid <- pathParam "id"
    titleParam <- formParam "title"
    result <- liftIO $ try (Core.updateTodo (tid, titleParam))
    case result of
      Left (ValidationError msg) -> do
        status badRequest400
        html $ Views.errorMessage msg
      Right Nothing -> do
        status notFound404
        html $ Views.errorMessage "Todo not found"
      Right (Just _) -> do
        todos <- liftIO Core.listTodos
        html $ Views.todoList todos