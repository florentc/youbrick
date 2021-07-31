{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui
Description : Main Terminal User Interface module
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Main Terminal User Interface including main rendering and main event handling
relying on the Brick library.
-}

module Tui
  ( runTui
  ) where

import qualified Database as D
import Control.Monad.IO.Class (liftIO)

import Lens.Micro.Platform ((^.), (&), (%~))

import qualified Data.Set as Set

import qualified Graphics.Vty as Vty
import Brick.Types
import Brick.BChan (BChan)
import qualified Brick.Main as Brick
import qualified Brick.AttrMap as Brick
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.List as Brick
import qualified Brick.Widgets.Border as Brick

import Tui.State
import Tui.Name (Name)
import qualified Tui.KeybindingsIndicator as KeybindingsIndicator
import qualified Tui.Actions as Action
import qualified Tui.Job as Job
import qualified Tui.ChannelList as WChannelList
import qualified Tui.VideoList as WVideoList
import qualified Tui.AddChannel as WAddChannel


-- | Initial state of the application bootstraped from a database of channels
-- and videos (intended to be read from a persistent file at startup) and a
-- Brick channel to receive events about jobs (adding/updating a channel)
-- termination. The TUI initially shows the channel list.
initialState :: D.Database -> BChan CustomEvent -> State
initialState database bchan = State
  { _sDatabase = database
  , _sBchan = bchan
  , _sView = View
    { _vActivity = ConsultChannelList
    , _vWidgetBank = WidgetBank
      { _wChannelList = WChannelList.new database Set.empty
      , _wVideoList = WVideoList.newWatchlistWidget database
      , _wAddChannel = WAddChannel.new
      }
    , _vReports = []
    , _vJobs = Set.empty
    }
  }

-- | A stack of Brick widgets of the current focused activity. Mostly only one
-- widget with the exception of the "add channel" activity which is a popup
-- window over the channel list.
drawActivity :: State -> [Widget Name]
drawActivity state =
  let widgetBank = state ^. sView . vWidgetBank
      activity = state ^. sView . vActivity
      widgetKeybindings = KeybindingsIndicator.draw activity
   in case activity of
        ConsultChannelList ->
          [ WChannelList.draw (widgetBank ^. wChannelList) Brick.<=> widgetKeybindings ]
        AddChannel ->
          [ WAddChannel.draw (widgetBank ^. wAddChannel)
          , Brick.withDefAttr attrUnfocused (WChannelList.draw (widgetBank ^. wChannelList))
            Brick.<=> widgetKeybindings ]
        ConsultVideoList _ ->
          [ WVideoList.draw (state ^. sDatabase) (widgetBank ^. wVideoList)
            Brick.<=> widgetKeybindings ]

-- | A widget for basic error reporting. In particular when a job (e.g. adding
-- a channel) fails (e.g. invalid URL).
drawError :: Report -> Widget Name
drawError (mJob, description) =
  Brick.centerLayer $
  Brick.borderWithLabel (Brick.txt " Error ") $
  Brick.hLimit 80 $
  Brick.vLimit 30 $
  case mJob of
    Nothing -> Brick.txtWrap description
    Just job -> (Brick.strWrap . show $ job) Brick.<=> Brick.txtWrap description

-- | Main stack of widget which consists of the current activity stack of
-- widgets and, if there are things to report, a stack of error reports (dialog
-- boxes) over.
draw :: State -> [Widget Name]
draw state =
  let activityWidgets = drawActivity state
      backgroundTaskWidget = Brick.str . show $ state ^. (sView . vJobs)
      errorWidgets = drawError <$> state ^. (sView . vReports)
   in if null errorWidgets
         then activityWidgets
         else errorWidgets <> (Brick.withDefAttr attrUnfocused <$> activityWidgets)

-- | A brick attribute for elements of the TUI that are not in focus.
attrUnfocused :: Brick.AttrName
attrUnfocused = Brick.attrName "unfocused"

-- | The attribute map: appearance implied by different Brick and custom
-- attribues. Here unfocused elements (less vibrant color) and selected
-- elements in lists (reverse background/foreground).
appAttrMap :: Brick.AttrMap
appAttrMap = Brick.attrMap Vty.defAttr
  [ (Brick.listSelectedAttr, Vty.withStyle Vty.currentAttr Vty.reverseVideo)
  , (attrUnfocused, Vty.withStyle Vty.currentAttr Vty.dim)
  ]

-- | The Brick application including main drawing function, event handler, and
-- attribute map.
app :: Brick.App State CustomEvent Name
app = Brick.App
  { Brick.appDraw = draw
  , Brick.appChooseCursor = Brick.showFirstCursor
  , Brick.appHandleEvent = handleEvent
  , Brick.appStartEvent = pure
  , Brick.appAttrMap = const appAttrMap
  }


-- | Build and launch the Brick application. It is initialized from a database
-- and makes use of a Brick channel for custom events: when network jobs
-- (adding and updating channels) end.
runTui :: D.Database -> IO D.Database
runTui database = do
  eventChan <- Brick.newBChan 10
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  endState <-
    Brick.customMain
    initialVty
    buildVty
    (Just eventChan)
    app
    (initialState database eventChan)
  return (endState ^. sDatabase)


-- | Main event handling.
handleEvent :: State -> BrickEvent Name CustomEvent -> EventM Name (Next State)
handleEvent s e =

  case e of

    -- Job events: a job has finished
    AppEvent (job, ending) ->
      let s' = Action.deleteJob job s
       in case ending of
            -- It ended in an error that must be reported
            Job.Failure description ->
                Brick.continue $ Action.reportJobError job description s'
            -- A channel id has been parsed from a URL: a regular channel
            -- update is triggered to fetch its content
            Job.ChannelIdParsed cid ->
              do
                let (job, s'') = Action.jobUpdateChannel cid True s'
                liftIO $ Job.startJob (s' ^. sBchan) job
                Brick.continue s''
            -- A channel has been updated
            Job.ChannelUpdated silent channel videos ->
                Brick.continue $ Action.updateChannel silent channel videos s'

    -- User input (vty events)
    VtyEvent vtyEvent ->

      -- If there are error reports on screen, discarding them with Enter or
      -- Esc is the only handled event.
      if not . null $ s ^. (sView . vReports)
      then case vtyEvent of
                Vty.EvKey Vty.KEnter [] -> Brick.continue $ s & sView . vReports %~ tail
                _ -> Brick.continue s

      -- Keybindings and action depend on the current activity
      else case s ^. (sView . vActivity) of

        ConsultChannelList ->
          case vtyEvent of
            Vty.EvKey Vty.KEsc []     -> Brick.halt s
            Vty.EvKey (Vty.KChar 'a') [] -> Brick.continue $ Action.openAddChannelPrompt s
            Vty.EvKey (Vty.KChar 'r') [] ->
              do
                let (jobs, s') = Action.jobsUpdateAllChannels s
                liftIO $ Job.startJobs (s ^. sBchan) jobs
                Brick.continue s'
            Vty.EvKey Vty.KEnter []      -> Brick.continue $ Action.browseSelectedChannelLastContent s
            Vty.EvKey (Vty.KChar 'b') [] -> do
                let (webbrowseChannel, s') = Action.webbrowseSelectedChannel s
                liftIO webbrowseChannel
                Brick.continue s'
            Vty.EvKey (Vty.KChar 'd') [] -> Brick.continue $ Action.deleteSelectedChannel s
            Vty.EvKey (Vty.KChar 'w') [] -> Brick.continue $ Action.setSelectedChannelWatched s
            Vty.EvKey (Vty.KChar 'l') []    -> Brick.continue $ Action.browseNotWatchedVideos s
            -- Delegate handling of other events to the widget's specific handler
            -- (e.g. navigating through a list with the arrow keys)
            _ -> Brick.continue =<< handleEventLensed s (sView . vWidgetBank . wChannelList) WChannelList.handleEvent vtyEvent

        ConsultVideoList _ ->
          case vtyEvent of
            Vty.EvKey Vty.KEsc [] -> Brick.continue $ Action.browseChannelList s
            Vty.EvKey Vty.KEnter [] -> do
                let (playVideo, s') = Action.playSelectedVideo s
                liftIO playVideo
                Brick.continue s'
            Vty.EvKey (Vty.KChar 'b') [] -> do
                let (webbrowseVideo, s') = Action.webbrowseSelectedVideo s
                liftIO webbrowseVideo
                Brick.continue s'
            Vty.EvKey (Vty.KChar 'w') [] -> Brick.continue $ Action.toggleSelectedVideoWatchStatus s
            -- Delegate handling of other events to the widget's specific handler
            -- (e.g. navigating through a list with the arrow keys)
            _ -> Brick.continue =<< handleEventLensed s (sView . vWidgetBank . wVideoList) WVideoList.handleEvent vtyEvent

        AddChannel ->
          case vtyEvent of
            Vty.EvKey Vty.KEsc [] -> Brick.continue $ Action.browseChannelList s
            Vty.EvKey Vty.KEnter [] ->
              do
                let (job, s') = Action.jobAddChannel s
                liftIO $ Job.startJob (s ^. sBchan) job
                Brick.continue s'
            -- Delegate handling of other events to the widget's specific handler
            -- (e.g. writing text in the text field)
            _ -> Brick.continue =<< handleEventLensed s (sView . vWidgetBank . wAddChannel) WAddChannel.handleEvent vtyEvent

    -- Ignored events
    _ -> Brick.continue s
