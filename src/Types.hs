{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics

data Todo = Todo
  { todoId :: Int,
    todoTitle :: Text,
    todoCreatedAt :: UTCTime,
    todoCompletedAt :: Maybe UTCTime,
    todoCompleted :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Todo where
  toJSON (Todo id' title createdAt completedAt completed) =
    object
      [ "id" .= id',
        "title" .= title,
        "created_at" .= createdAt,
        "completed_at" .= completedAt,
        "completed" .= completed
      ]

instance FromJSON Todo

-- This is crucial for PostgreSQL.Simple to work
instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field <*> field

newtype NewTodo = NewTodo
  { title :: Text
  }
  deriving (Show, Generic)

instance ToJSON NewTodo

instance FromJSON NewTodo