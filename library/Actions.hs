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

withState :: (Track -> IO ()) -> IO ()
withState action = do
  st <- getState
  case st of
    Left e -> error e
    Right state -> action state

pauseTask :: String -> IO ()
pauseTask taskName =
  withState $ \state -> do
    let foundTask = find (\x -> x ^. name == taskName) (state ^. tasks)
    case foundTask of
      Nothing -> taskNotFound taskName
      Just _ -> do
        time <- currentTime
        writeState $ state & tasks %~ map (
                      \x -> if x ^. name == taskName
                        then x & checkpoints %~ \y -> y |> time
                        else x)
        taskPaused taskName

startTask :: String -> IO ()
startTask taskName =
  withState $ \state -> do
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
deleteTask taskName =
  withState $ \state -> do
      let foundTask = find (\x -> x ^. name == taskName) (state ^. tasks)
      case foundTask of
        Nothing -> taskNotFound taskName
        Just _ -> do
          writeState $ state & tasks %~ filter (\x -> x ^. name /= taskName)
          taskDeleted taskName

printStatus :: Maybe String -> IO ()
printStatus task =
  withState $ \state ->
    case task of
      Nothing -> if null (state ^. tasks)
        then noTasks
        else mapM_ sumarizeStatus $ state ^. tasks
      Just taskName -> do
        let foundTask = find (\x -> x ^. name == taskName) (state ^. tasks)
        case foundTask of
          Nothing -> taskNotFound taskName
          Just t -> mapM_ sumarizeStatus [t]
