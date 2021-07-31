{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Tui.State
Description : State of the application
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Data structures of the state of the application including the database and TUI.
-}

module Tui.State where

import Data.Text (Text)
import Data.Set (Set)

import Lens.Micro.Platform

import Brick.BChan (BChan)

import qualified Database as D

import Tui.ChannelList (WidgetChannelList)
import Tui.VideoList (WidgetVideoList)
import Tui.AddChannel (WidgetAddChannel)
import Tui.Job (Job)
import qualified Tui.Job as Job

-- | Custom events (here job ending events) in addition to Brick internal events
type CustomEvent = Job.Event

-- | Info or error to report to the user (e.g. about the end of a job)
type Report = (Maybe Job, Text)

-- | State of the app
data State = State
  { _sDatabase :: D.Database -- ^ Current database of channels and videos
  , _sBchan :: BChan CustomEvent -- ^ Brick channel to receive custom events
  , _sView :: View -- ^ Current state of the TUI: current activity and widgets
  }

-- | State of the view (including current activity and brick widgets)
data View = View
  { _vActivity :: Activity -- ^ Current focused activity
  , _vWidgetBank :: WidgetBank -- ^ Brick widgets available to view
  , _vReports :: [Report] -- ^ List of infos/errors to report to the user
  , _vJobs :: Set Job -- ^ Current running jobs (adding or updating channels)
  }

-- | Current focused activity
data Activity = ConsultChannelList
              | AddChannel
              | ConsultVideoList VideoListType

-- | Whether a video list is about latest content on a channel or general
-- watchlist
data VideoListType = VideoListToWatch
                   | VideoListChannel D.ChannelId


-- | All the Brick widgets available as building blocks for the TUI. In
-- practice almost always one is used at a time (the exception being the "add
-- channel" widget which is a popup window over the "channel list" widget). In
-- the future, "channel list" and "video lsit" might be displayed as two panes
-- alongside each other.
data WidgetBank = WidgetBank
  { _wChannelList :: WidgetChannelList
  , _wVideoList :: WidgetVideoList
  , _wAddChannel :: WidgetAddChannel
  }

makeLenses ''State
makeLenses ''View
makeLenses ''WidgetBank
