{-# LANGUAGE OverloadedStrings #-}

-- Main.hs
module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Core (ValidationError (..))
import Core qualified
import HTMXRoutes qualified
import Network.HTTP.Types.Status
import Response (ResponseDTO (..))
import Types (NewTodo (..))
import Web.Scotty

main :: IO ()
main = do
  loadFile defaultConfig

  scotty 3000 $ do
    -- Original API endpoints
    get "/" $
      text "Todo API!!!"

    post "/todos" $ do
      NewTodo {title = t} <- jsonData
      result <- liftIO $ try (Core.createTodo t)
      case result of
        Left (ValidationError msg) -> do
          status badRequest400
          json $ ResponseDTO {message = msg, data_ = ()}
        Right todo -> do
          status created201
          json $ ResponseDTO {message = "Todo created", data_ = todo}

    get "/todos" $ do
      todos <- liftIO Core.listTodos
      json $ ResponseDTO {message = "Todo fetched", data_ = todos}

    get "/todos/:id" $ do
      tid <- pathParam "id"
      mtodo <- liftIO $ Core.getTodo tid
      case mtodo of
        Nothing -> do
          status notFound404
          json $ ResponseDTO {message = "Todo not found", data_ = ()}
        Just todo ->
          json $ ResponseDTO {message = "Todo fetched", data_ = todo}

    patch "/todos/:id" $ do
      tid <- pathParam "id"
      NewTodo {title = t} <- jsonData
      result <- liftIO $ try (Core.updateTodo (tid, t))
      case result of
        Left (ValidationError msg) -> do
          status badRequest400
          json $ ResponseDTO {message = msg, data_ = ()}
        Right Nothing -> do
          status notFound404
          json $ ResponseDTO {message = "Todo not found", data_ = ()}
        Right (Just todo) ->
          json $ ResponseDTO {message = "Todo updated", data_ = todo}

    patch "/todos/:id/complete" $ do
      tid <- pathParam "id"
      mtodo <- liftIO $ Core.markDone tid
      case mtodo of
        Nothing -> do
          status notFound404
          json $ ResponseDTO {message = "Todo not found", data_ = ()}
        Just todo -> json $ ResponseDTO {message = "Todo marked as complete", data_ = todo}

    delete "/todos/:id" $ do
      tid <- pathParam "id"
      ok <- liftIO $ Core.removeTodo tid
      if ok
        then do
          status noContent204
          json $ ResponseDTO {message = "Todo deleted", data_ = ()}
        else do
          status notFound404
          json $ ResponseDTO {message = "Todo not found", data_ = ()}

    -- Add HTMX routes
    HTMXRoutes.addHTMXRoutes