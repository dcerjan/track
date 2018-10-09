{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Task where

import GHC.Generics
import Data.Text hiding (drop, length)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Control.Lens

data Task = Task  { _name :: String
                  , _checkpoints :: [Int]
                  } deriving (Show, Generic)

makeLenses ''Task
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Task

isPaused :: Task -> Bool
isPaused task = even . length $ task ^. checkpoints