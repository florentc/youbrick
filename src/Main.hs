{-|
Module      : Main
Description : Main module
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Management of the persistent database file and TUI launch.
-}

module Main
  ( main
  ) where

import qualified Config
import Database as D
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import Text.Read (readMaybe)
import Tui

-- | Main program: fetch the database from the persistent database file, run
-- the Brick terminal user interface from which the user will interact with the
-- database, and eventually save the changes to the persistent file.
main :: IO ()
main = do
  mDatabase <- initializeDatabase
  case mDatabase of
    Nothing -> IO.hPutStrLn IO.stderr "Corrupted database file.'"
    Just database -> do
      database <- runTui database
      writeDatabase database

-- | Open and parse the persistent database file, or start from a new empty
-- database if it does not exist. Returns Nothing when the database cannot be
-- parsed correctly from the file.
initializeDatabase :: IO (Maybe D.Database)
initializeDatabase = do
  fileExists <- Directory.doesFileExist =<< getDatabaseFilePath
  if fileExists
    then readMaybe <$> (readFile =<< getDatabaseFilePath)
    else return (Just D.empty)

-- | Dump the database content to the persistent database file. The file and
-- parent directories are created if they do not exist. Due to lazy IO, the
-- database is first dumped to a temporary file which then replaces the
-- original file.
writeDatabase :: D.Database -> IO ()
writeDatabase database = do
  tempDirectory <- Directory.getTemporaryDirectory
  (tempFilepath, tempHandle) <-
    IO.openTempFile tempDirectory Config.tempDatabaseFileName
  Directory.createDirectoryIfMissing True =<< getDataDirectory
  databaseFilePath <- getDatabaseFilePath
  IO.hPutStr tempHandle (show database)
  IO.hClose tempHandle
  Directory.copyFile tempFilepath databaseFilePath
  Directory.removeFile tempFilepath

-- | Get the directory where to save the persistent database file.
getDataDirectory :: IO FilePath
getDataDirectory =
  Directory.getXdgDirectory Directory.XdgData Config.applicationName

-- | Get the file path of the persistent database file.
getDatabaseFilePath :: IO FilePath
getDatabaseFilePath = (</> Config.databaseFileName) <$> getDataDirectory
