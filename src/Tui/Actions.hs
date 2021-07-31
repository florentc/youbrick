{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.Actions
Description : Operations triggered from the TUI
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Operations with an effect on the state of the database or the TUI that results
from user interactions.
-}

module Tui.Actions
  ( deleteJob
  , jobAddChannel
  , jobUpdateChannel
  , jobsUpdateAllChannels
  , reportError
  , reportJobError
  , openAddChannelPrompt
  , browseChannelList
  , browseSelectedChannelLastContent
  , browseNotWatchedVideos
  , setSelectedChannelWatched
  , updateChannel
  , deleteSelectedChannel
  , playSelectedVideo
  , webbrowseSelectedVideo
  , webbrowseSelectedChannel
  , toggleSelectedVideoWatchStatus
  ) where

import Config (viewerApplication, browserApplication)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Database as D

import Control.Monad (void)
import System.Process (createProcess, proc, std_in, std_out, std_err, StdStream(NoStream))

import Lens.Micro.Platform

import Tui.State
import Tui.Job (Job)
import qualified Tui.Job as Job
import qualified Tui.ChannelList as WChannelList
import qualified Tui.VideoList as WVideoList
import qualified Tui.AddChannel as WAddChannel


-- | Report a direct error.
reportError :: Text -> State -> State
reportError description = sView . vReports %~ ((Nothing, description) :)

-- | Report an error related to a job failure (e.g. network error when fetching
-- new content).
reportJobError :: Job -> Text -> State -> State
reportJobError job description = sView . vReports %~ ((Just job, description) :)

addJob :: Job -> State -> State
addJob = (sView . vJobs %~) . Set.insert

addJobs :: Set Job -> State -> State
addJobs = (sView . vJobs %~) . Set.union

deleteJob :: Job -> State -> State
deleteJob = (sView . vJobs %~) . Set.delete

-- | Clear the text field of the "add channel" widget.
emptyAddChannelPrompt :: State -> State
emptyAddChannelPrompt = sView . vWidgetBank . wAddChannel .~ WAddChannel.new

-- | Add a new channel in a parallel job.
jobAddChannel :: State -> (Job, State)
jobAddChannel s =
  let job = Job.AddChannel $ WAddChannel.content (s ^. sView . vWidgetBank . wAddChannel)
   in (job, s & browseChannelList . addJob job . emptyAddChannelPrompt)

-- | Update (fetch new content) a channel in a parallel job.
jobUpdateChannel :: D.ChannelId -> Bool -> State -> (Job, State)
jobUpdateChannel cid silent s = let job = Job.UpdateChannel cid silent in (job, addJob job s)

-- | Update (fetch new content) all channels each in a parallel job.
jobsUpdateAllChannels :: State -> (Set Job, State)
jobsUpdateAllChannels s =
  let jobs = Set.map (\cid -> Job.UpdateChannel cid False) (D.channelIds $ s ^. sDatabase)
      s' = s & addJobs jobs
      updatingChannels = Job.updatingChannels (s' ^. sView . vJobs)
      updateChannelList = sView . vWidgetBank . wChannelList %~ WChannelList.update (s' ^. sDatabase) updatingChannels
   in (jobs, s' & updateChannelList)

-- | Update the state of the app given fresh content about a channel. An update
-- can be silent which means new content will already be considered watched.
-- This is used when adding a new channel.
updateChannel :: Bool                 -- ^ Whether to update "silently"
              -> D.Channel            -- ^ Channel id and its maybe new title
              -> Set D.VideoImport  -- ^ Raw info about latest content
              -> State
              -> State
updateChannel silent channel videos s =
  let database' =
          (if silent then D.updateChannelSilently else D.updateChannel)
          channel videos (s ^. sDatabase)
      updatingChannels = Job.updatingChannels (s ^. sView . vJobs)
   in s
   & sDatabase .~ database'
   & sView . vWidgetBank . wChannelList %~ WChannelList.update database' updatingChannels
   & sView . vWidgetBank . wVideoList %~
     case s ^. sView . vActivity of
       ConsultVideoList (VideoListChannel cid) ->
         if cid == D.channelId channel then WVideoList.update database' else id
       ConsultVideoList VideoListToWatch ->
         let videos' = D.lookupNotWatched database'
          in WVideoList.update database'
       _ -> id

-- | Open and focus the "add channel" widget.
openAddChannelPrompt :: State -> State
openAddChannelPrompt = sView . vActivity .~ AddChannel

-- | Current selected channel in the channel list.
selectedChannelId :: State -> Maybe D.ChannelId
selectedChannelId = WChannelList.selectedChannel . (^. sView . vWidgetBank . wChannelList)

-- | Open the list of latest video on the currently selected channel.
browseSelectedChannelLastContent :: State -> State
browseSelectedChannelLastContent s =
    case selectedChannelId s of
      Nothing -> s & reportError "No channel selected."
      Just cid ->
          case WVideoList.newChannelWidget cid (s ^. sDatabase) of
            Nothing -> s & reportError "Could not browse channel content."
            Just w -> s
              & sView . vActivity .~ ConsultVideoList (VideoListChannel cid)
              & sView . vWidgetBank . wVideoList .~ w

-- | Open the list of videos that have not been watched yet.
browseNotWatchedVideos :: State -> State
browseNotWatchedVideos s =
  let videos = D.lookupNotWatched (s ^. sDatabase)
   in s
   & sView . vActivity .~ ConsultVideoList VideoListToWatch
   & sView . vWidgetBank . wVideoList .~ WVideoList.newWatchlistWidget (s ^. sDatabase)

-- | Remove selected channel from the database.
deleteSelectedChannel :: State -> State
deleteSelectedChannel s =
  case selectedChannelId s of
    Nothing -> s & reportError "No channel selected."
    Just cid ->
      let database' = D.removeChannel cid (s ^. sDatabase)
          updatingChannels = Job.updatingChannels (s ^. sView . vJobs)
       in s
       & sDatabase .~ database'
       & sView . vWidgetBank . wChannelList %~ WChannelList.update database' updatingChannels

-- | Go back to the list of channels.
browseChannelList :: State -> State
browseChannelList = sView . vActivity .~ ConsultChannelList

-- | Currently selected video in the video list.
selectedVideoId :: State -> Maybe D.VideoId
selectedVideoId = WVideoList.selectedVideo . (^. sView . vWidgetBank . wVideoList)

-- | Toggle the watch status of the selected video.
toggleSelectedVideoWatchStatus :: State -> State
toggleSelectedVideoWatchStatus s =
  case selectedVideoId s of
    Nothing -> s & reportError "No video selected."
    Just vid ->
      let database' = D.toggleWatched vid (s ^. sDatabase)
          updatingChannels = Job.updatingChannels (s ^. sView . vJobs)
       in s
       & sDatabase .~ database'
       & sView . vWidgetBank . wVideoList %~ WVideoList.updateSelectedItem database'
       & sView . vWidgetBank . wChannelList %~ WChannelList.update database' updatingChannels

-- | Mark the selected video watched.
setSelectedVideoWatched :: State -> State
setSelectedVideoWatched s =
  case selectedVideoId s of
    Nothing -> s & reportError "No video selected."
    Just vid ->
      let database' = D.setWatched (Set.singleton vid) (s ^. sDatabase)
          updatingChannels = Job.updatingChannels (s ^. sView . vJobs)
       in s
       & sDatabase .~ database'
       & sView . vWidgetBank . wVideoList %~ WVideoList.updateSelectedItem database'
       & sView . vWidgetBank . wChannelList %~ WChannelList.update database' updatingChannels

-- | Mark all the videos of the selected channel watched.
setSelectedChannelWatched :: State -> State
setSelectedChannelWatched s =
  case selectedChannelId s of
    Nothing -> s & reportError "No channel selected."
    Just cid ->
        let videos = D.lookupLatestContentOnChannel cid (s ^. sDatabase)
            database' = D.setWatched (Set.map D.videoId videos) (s ^. sDatabase)
            updatingChannels = Job.updatingChannels (s ^. sView . vJobs)
         in s & sDatabase .~ database'
              & sView . vWidgetBank . wChannelList %~ WChannelList.update database' updatingChannels

-- | Open the selected video with the video player in a new process.
playSelectedVideo :: State -> (IO (), State)
playSelectedVideo s =
  case selectedVideoId s of
    Nothing -> (return (), s & reportError "No video selected.")
    Just vid ->
      ( void $ createProcess (proc viewerApplication [Text.unpack $ D.genVideoUrl vid])
        {std_in = NoStream, std_err = NoStream, std_out = NoStream}
      , setSelectedVideoWatched s)

-- | Open the selected video with the web browser.
webbrowseSelectedVideo :: State -> (IO (), State)
webbrowseSelectedVideo s =
  case selectedVideoId s of
    Nothing -> (return (), s & reportError "No video selected.")
    Just vid ->
      ( void $ createProcess (proc browserApplication [Text.unpack $ D.genVideoUrl vid])
        {std_in = NoStream, std_err = NoStream, std_out = NoStream}
      , s)

-- | Open the selected channel in the web browser.
webbrowseSelectedChannel :: State -> (IO (), State)
webbrowseSelectedChannel s =
  case selectedChannelId s of
    Nothing -> (return (), s & reportError "No channel selected.")
    Just cid ->
      ( void $ createProcess (proc browserApplication [Text.unpack $ D.genChannelUrl cid])
        {std_in = NoStream, std_err = NoStream, std_out = NoStream}
      , s)
