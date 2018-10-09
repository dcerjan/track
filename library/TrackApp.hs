{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE OverloadedStrings#-}

module TrackApp (main) where

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import System.Directory

import Actions
import Models.Track

newtype Opts = Opts { optCommand :: Command }

data Command
  = Start String
  | Pause String
  | Delete String
  | View String

main :: IO ()
main = do
  -- check for first run
  dir <- getHomeDirectory
  fileExists <- doesFileExist $ dir ++ "/.track.json"
  unless fileExists $ writeState $ Track []

  args <- getArgs

  if null args
    then printStatus Nothing
    else do
      (opts :: Opts) <- execParser optsParser
      case optCommand opts of
        Start taskName -> startTask taskName
        Pause taskName -> pauseTask taskName
        Delete taskName -> deleteTask taskName
        View taskName -> printStatus $ Just taskName

  where
    optsParser :: ParserInfo Opts
    optsParser = info
      ( helper <*>
        versionOption <*>
        programOptions )
      ( fullDesc <>
        progDesc "a stupid time tracker" <>
        header "a stupid time tracker" )

    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")

    programOptions :: Parser Opts
    programOptions = Opts <$> hsubparser (
      createCommand <>
      pauseCommand <>
      deleteCommand <>
      viewCommand )

    createCommand :: Mod CommandFields Command
    createCommand = command "start" (info createOptions (progDesc "Start a new task"))
    createOptions :: Parser Command
    createOptions = Start <$> strArgument (metavar "NAME" <> help "Name of the task to create")

    pauseCommand :: Mod CommandFields Command
    pauseCommand = command "pause" (info pauseOptions (progDesc "Pause a task"))
    pauseOptions :: Parser Command
    pauseOptions = Pause <$> strArgument (metavar "NAME" <> help "Name of the task to pause")

    deleteCommand :: Mod CommandFields Command
    deleteCommand = command "delete" (info deleteOptions (progDesc "Delete a task"))
    deleteOptions :: Parser Command
    deleteOptions = Delete <$> strArgument (metavar "NAME" <> help "Name of the task to delete")

    viewCommand :: Mod CommandFields Command
    viewCommand = command "view" (info viewOptions (progDesc "View tasks"))
    viewOptions :: Parser Command
    viewOptions = View <$> strArgument (metavar "NAME" <> help "Name of the task to view")
