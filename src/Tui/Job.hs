{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.Job
Description : Asynchronous jobs
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Management of asynchronous actions involving online requests: adding a new
channel and updating a channel. These actions are done in another thread to
avoid blocking the TUI.
-}

module Tui.Job
  ( Job (..)
  , Ending (..)
  , Event
  , startJob
  , startJobs
  , updatingChannels
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent (forkFinally)
import Control.Exception (try, displayException, SomeException)

import Brick.BChan (BChan)
import qualified Brick.BChan as Brick

import qualified Database as D
import qualified Network as N

-- Aynchronous jobs
data Job =
  -- | Fetch a new channel id from a URL
    AddChannel Text
  -- | Fetch latest info and content of a channel. New content may be
  -- automatically tagged as seen ("silent" flag) when the update happens to a
  -- newly added channel. This avoids flooding the watchlist with old videos. 
  | UpdateChannel D.ChannelId Bool
    deriving (Eq, Ord, Show)

-- How a job may end
data Ending =
  -- | The job failed for a given reason
    Failure Text
  -- | The channel id has been parsed from a webpage
  | ChannelIdParsed D.ChannelId
  -- | The channel has been updated
  | ChannelUpdated
    Bool -- ^ Whether the update is silent (mark new content as watched)
    D.Channel -- ^ The channel (with its title that might have changed)
    (Set D.VideoImport) -- ^ The raw info about the latest content

-- The end of a job
type Event = (Job, Ending)

-- Start a job in another thread and report the ending event in the given Brick
-- channel.
startJob :: BChan Event -> Job -> IO ()
startJob bchan job =
  let andThen eitherEnding =
        do
          Brick.writeBChan bchan
            ( job
            , case eitherEnding of
                Left e -> Failure $ Text.pack . displayException $ (e :: SomeException)
                Right ending -> ending
            )
   in () <$ forkFinally (doJob job) andThen

-- Start several jobs in other threads and report the ending events in the
-- given Brick channel.
startJobs :: BChan Event -> Set Job -> IO ()
startJobs bchan tasks = sequence_ (startJob bchan <$> Set.toList tasks)

-- The job thread
doJob :: Job -> IO Ending
doJob (AddChannel url) =
  do
    eCid <- try $ N.extractChannelId url
    return $ case eCid of
      Left e -> Failure $ Text.pack . displayException $ (e :: SomeException)
      Right cid -> ChannelIdParsed cid
doJob (UpdateChannel cid silent) =
  do
    emFeed <- try $ N.getFeed cid
    return $ case emFeed of
      Left e -> Failure $ Text.pack . displayException $ (e :: SomeException)
      Right Nothing -> Failure "Could not parse feed."
      Right (Just feed) ->
        let title = N.parseTitle feed
            videos = N.parseVideos feed
         in ChannelUpdated silent (D.Channel cid title) (Set.fromList videos)

-- Tool function to see in currently running jobs which channels are being
-- updated. This is used to display update icons in the channel list.
updatingChannels :: Set Job -> Set D.ChannelId
updatingChannels = Set.unions . Set.map f
  where f (UpdateChannel cid _) = Set.singleton cid
        f _ = Set.empty
