{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Response where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, defaultOptions, genericToJSON, fieldLabelModifier)

-- Define the ResponseDTO data type
data ResponseDTO a = ResponseDTO
  { 
  message :: String
  , data_ :: a
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (ResponseDTO a) where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "data_" -> "data"
    other   -> other }