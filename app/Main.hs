{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.IO.Class (liftIO)
import Core qualified
import Response (ResponseDTO(..))
import Network.HTTP.Types.Status (noContent204, notFound404, created201)
import Types (NewTodo (..))
import Web.Scotty

main :: IO ()
main = do
  -- Load environment variables from .env file
  loadFile defaultConfig

  scotty 3000 $ do
    get "/" $ do
      text "Todo API!!!"

    post "/todos" $ do
      NewTodo {title = t} <- jsonData
      todo <- liftIO $ Core.createTodo t
      status created201
      json $ ResponseDTO { message = "Todo created", data_ = todo }

    get "/todos" $ do
      todos <- liftIO Core.listTodos
      liftIO $ putStrLn "Here"
      json $ ResponseDTO { message = "Todos fetched", data_ = todos }

    get "/todos/:id" $ do
      tid <- pathParam "id"
      mtodo <- liftIO $ Core.getTodo tid
      case mtodo of
        Nothing -> do
          status notFound404
          json $ ResponseDTO { message = "Todo not found", data_ = () }
        Just todo -> json $ ResponseDTO { message = "Todo fetched", data_ = todo }

    patch "/todos/:id" $ do
      tid <- pathParam "id"
      NewTodo {title} <- jsonData
      mtodo <- liftIO $ Core.updateTodo (tid, title)
      case mtodo of
        Nothing -> do
          status notFound404
          json $ ResponseDTO { message = "Todo not found", data_ = () }
        Just todo -> json $ ResponseDTO { message = "Todo updated", data_ = todo }

    patch "/todos/:id/complete" $ do
      tid <- pathParam "id"
      mtodo <- liftIO $ Core.markDone tid
      case mtodo of
        Nothing -> do
          status notFound404
          json $ ResponseDTO { message = "Todo not found", data_ = () }
        Just todo -> json $ ResponseDTO { message = "Todo marked as complete", data_ = todo }

    delete "/todos/:id" $ do
      tid <- pathParam "id"
      ok <- liftIO $ Core.removeTodo tid
      if ok
        then do
          status noContent204
          json $ ResponseDTO { message = "Todo deleted", data_ = () }
        else do
          status notFound404
          json $ ResponseDTO { message = "Todo not found", data_ = () }