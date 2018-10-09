module Actions where

import Data.List
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I
import Data.ByteString as B hiding (filter, map, null, find)
import Control.Lens
import System.Directory

import Models.Track
import Models.Task
import Utils

type Action = (Track -> Track)

getState :: IO (Either String Track)
getState = do
  dir <- getHomeDirectory
  (eitherDecodeStrict' <$> B.readFile (dir ++ "/.track.json")) :: IO (Either String Track)

writeState :: Track -> IO ()
writeState track = do
  dir <- getHomeDirectory
  I.writeFile (dir ++ "/.track.json") (encodeToLazyText track)

pauseTask :: String -> IO ()
pauseTask taskName = do
  st <- getState
  case st of
    Left e -> error e
    Right state -> do
      let foundTasks = (state & tasks %~ filter (\x -> x ^. name == taskName)) ^. tasks
      if null foundTasks
        then taskNotFound taskName
        else do
          time <- currentTime
          writeState $ state & tasks %~ map (
                        \x -> if x ^. name == taskName
                          then x & checkpoints %~ \y -> y |> time
                          else x)
          taskPaused taskName

startTask :: String -> IO ()
startTask taskName = do
  st <- getState
  case st of
    Left e -> error e
    Right state -> do
      time <- currentTime
      let foundTask = find (\x -> x ^. name == taskName) (state ^. tasks)
      case foundTask of
        Nothing -> do
          writeState $ state & tasks %~ \x -> x |> Task taskName [time]
          taskStarted taskName
        Just t -> if isPaused t
          then do
            writeState $ state & tasks %~ map (
                        \x -> if x ^. name == taskName
                          then x & checkpoints %~ \y -> y |> time
                          else x)
            taskStarted taskName
          else
            taskAlreadyInProgress taskName

deleteTask :: String -> IO ()
deleteTask taskName = do
  st <- getState
  case st of
    Left e -> error e
    Right state -> do
      let foundTasks = (state & tasks %~ filter (\x -> x ^. name == taskName)) ^. tasks
      if null foundTasks
        then taskNotFound taskName
        else do
          writeState $ state & tasks %~ filter (\x -> x ^. name /= taskName)
          taskDeleted taskName

printStatus :: Maybe String -> IO ()
printStatus task = do
  st <- getState
  case st of
    Left e -> error e
    Right state -> case task of
      Nothing -> if null (state ^. tasks)
        then noTasks
        else mapM_ sumarizeStatus $ state ^. tasks
      Just taskName -> do
        let t = (state & tasks %~ filter (\x -> x ^. name == taskName)) ^. tasks
        if Prelude.null t
          then taskNotFound taskName
          else mapM_ sumarizeStatus t
