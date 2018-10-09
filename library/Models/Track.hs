{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Track where

import GHC.Generics
import Data.Text hiding (drop)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Control.Lens

import Models.Task

newtype Track = Track { _tasks :: [Task]
                      } deriving (Show, Generic)

makeLenses ''Track
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Track
