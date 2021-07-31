{-|
Module      : Config
Description : Configurable constants
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Configurable names and external applications.
-}

module Config
  ( applicationName
  , viewerApplication
  , browserApplication
  , databaseFileName
  , tempDatabaseFileName
  ) where

import System.Directory as Directory
import System.FilePath ((</>))

-- | Name of the app (used as a directory name where to store the database).
applicationName :: String
applicationName = "youbrick"

-- | Video viewer used to open video URLS.
viewerApplication :: String
viewerApplication = "mpv"

-- | Web browser used to browse channel homepages and video pages.
browserApplication :: String
browserApplication = "firefox"

-- | Name of the persistent database file. 
databaseFileName :: String
databaseFileName = "database"

-- | Name of the persistent database temporary file (see `writeDatabase` in the
-- `Main` module).
tempDatabaseFileName :: FilePath
tempDatabaseFileName = Config.applicationName <> "-" <> Config.databaseFileName
