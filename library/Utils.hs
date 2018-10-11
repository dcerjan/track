module Utils where

import Data.UnixTime
import Control.Lens
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Models.Task

currentTime :: IO Int
currentTime = fromEnum . utSeconds <$> getUnixTime

diff :: Task -> IO Int
diff t = do
  time <- currentTime
  return $ sumDiffs $ t ^. checkpoints ++ [time | not $ isPaused t]
  where
    sumDiffs (a:b:rest) = (b - a) + sumDiffs rest
    sumDiffs _ = 0

toHMS :: Int -> (Int, Int, Int)
toHMS seconds = (quot seconds 3600, flip mod 60 $ quot seconds 60, mod seconds 60)

timeSpent :: Task -> IO (Int, Int, Int)
timeSpent t = do
  total <- diff t
  return $ toHMS total

sumarizeStatus :: Task -> IO ()
sumarizeStatus task = do
  (hours, minutes, seconds) <- timeSpent task
  let taskName = task ^. name
  let g = if isPaused task then green else dullgreen
  let w = if isPaused task then white else dullwhite
  let c = if isPaused task then cyan else dullcyan
  putDoc $ c (text "Task")
        <+> w (text "[") <> g (text taskName) <> w (text "]")
        <+> w (text "[") <> g (text (if isPaused task then "Paused" else "In Progress")) <> w (text "]")
        <+> w (text "::")
        <+> g (text $ show hours) <> w (text "h")
        <+> g (text $ show minutes) <> w (text "m")
        <+> g (text $ show seconds) <> w (text "s")
        <> linebreak

taskNotFound :: String -> IO ()
taskNotFound taskName =
  putDoc $ red (text "Task")
        <+> white (text "[") <> green (text taskName) <> white (text "]")
        <+> red (text "does not exist")
        <> linebreak

noTasks :: IO ()
noTasks =
  putDoc $ white (text "There are no logged tasks")
        <> linebreak

taskDeleted :: String -> IO ()
taskDeleted taskName =
  putDoc $ red (text "Task")
        <+> white (text "[") <> red (text taskName) <> white (text "]")
        <+> red (text "deleted")
        <> linebreak

taskStarted :: String -> IO ()
taskStarted taskName =
  putDoc $ green (text "Task")
        <+> white (text "[") <> green (text taskName) <> white (text "]")
        <+> green (text "started")
        <> linebreak

taskPaused :: String -> IO ()
taskPaused taskName =
  putDoc $ green (text "Task")
        <+> white (text "[") <> green (text taskName) <> white (text "]")
        <+> green (text "paused")
        <> linebreak

taskAlreadyInProgress :: String -> IO ()
taskAlreadyInProgress taskName =
  putDoc $ green (text "Task")
        <+> white (text "[") <> green (text taskName) <> white (text "]")
        <+> green (text "is already in progress")
        <> linebreak