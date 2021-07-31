{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.VideoList
Description : Widget for video lists
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Management of the video list widget. It has two variants: a global watchlist
(videos that have not been watched yet), and a detailed list of videos on a
specific channel.
-}

module Tui.VideoList
  ( WidgetVideoList
  , newChannelWidget
  , newWatchlistWidget
  , update
  , updateSelectedItem
  , selectedVideo
  , draw
  , handleEvent
  ) where

import qualified Database as D
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Graphics.Vty.Input.Events as Vty (Event) 
import Brick.Types (Widget, EventM)
import qualified Brick.Types as Brick (Padding(Max))
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.List as Brick
import qualified Brick.Widgets.Border as Brick

import Lens.Micro.Platform ((^.))

import Tui.Name (Name)
import qualified Tui.Name as Name
import Tui.Splittable

-- | Variant of video list.
data VideoListType = ChannelContent D.Channel -- ^ Latest content of a channel
                   | Watchlist -- ^ Global watchlist regardless of the channel

-- | Widget for video lists.
data WidgetVideoList = WidgetVideoList VideoListType (Brick.GenericList Name [] D.Video)

-- | Native Brick list widget with videos.
newBrickWidget :: [D.Video] -> Brick.GenericList Name [] D.Video
newBrickWidget videos = Brick.list Name.VideoList videos 1

-- | New video list widget dedicated to a channel in the database or Nothing if
-- the channel is not in the database.
newChannelWidget :: D.ChannelId -> D.Database -> Maybe WidgetVideoList
newChannelWidget cid db =
    do
        channel <- D.lookupChannel cid db
        let brickWidget = newBrickWidget (D.lookupLatestContentOnChannelByDate cid db)
        return $ WidgetVideoList (ChannelContent channel) brickWidget

-- | New video list widget consisting of all videos that remain to be watched.
newWatchlistWidget :: D.Database -> WidgetVideoList
newWatchlistWidget db =
    WidgetVideoList Watchlist (newBrickWidget (D.lookupNotWatchedByDate db))

-- | Change the selection to a given video.
moveToElement :: D.Video -> WidgetVideoList -> WidgetVideoList
moveToElement v (WidgetVideoList listType brickWidget) =
    WidgetVideoList listType (Brick.listMoveToElement v brickWidget)

-- | Currently selected item.
selectedVideo :: WidgetVideoList -> Maybe D.VideoId
selectedVideo (WidgetVideoList _ brickWidget) = (\(_, v) -> D.videoId v) <$> Brick.listSelectedElement brickWidget

-- | Update the video list to fit the database. That is make a new list and
-- move to the previously selected element if possible. Returns nothing if the
-- video list is about a channel that is no longer in the database (corrupted
-- database).
updateMaybe :: D.Database -> WidgetVideoList -> Maybe WidgetVideoList
updateMaybe db (WidgetVideoList Watchlist brickWidget) =
    maybe id moveToElement (snd <$> Brick.listSelectedElement brickWidget) <$>
        Just (newWatchlistWidget db)
updateMaybe db (WidgetVideoList (ChannelContent c) brickWidget) =
    maybe id moveToElement (snd <$> Brick.listSelectedElement brickWidget) <$>
        newChannelWidget (D.channelId c) db

-- | Update the video list to fit the database. That is make a new list and
-- move to the previously selected element if possible. Does not change
-- anything if the video list is about a channel that is no longer in the
-- database (corrupted database).
update :: D.Database -> WidgetVideoList -> WidgetVideoList
update db w = fromMaybe w (updateMaybe db w)

--  | Update, when applicable, the selected item in the list according to the
--  database. This is used for instance when toggling the watch status of the
--  selected video.
updateSelectedItem :: D.Database -> WidgetVideoList -> WidgetVideoList
updateSelectedItem db w@(WidgetVideoList listType brickWidget) =
  fromMaybe w $
    do
      vid <- selectedVideo w
      v <- D.lookupVideo vid db
      return $ let modify x = if D.videoId x == vid then v else x
                in WidgetVideoList listType (Brick.listModify modify brickWidget)

-- | Render the publication date of a video.
drawDate :: D.Video -> Widget Name
drawDate v =
    Brick.txt
    $ Text.pack
    $ Time.formatTime Time.defaultTimeLocale "%a %d %h" (D.runPublicationTime . D._vPublicationTime $ v)

-- | Render the title of the channel where a video is published.
drawChannelTitle :: D.Database -> D.Video -> Widget Name
drawChannelTitle db v =
    maybe
    (Brick.txt (D.runChannelId $ v ^. D.vChannelId))
    (Brick.txtWrap . D.runChannelTitle . D.channelTitle)
    (D.lookupChannel (v ^. D.vChannelId) db)

-- | Render an item of the video list as a single line with the video title and
-- its publication date.
drawItemSimple :: D.Video -> Widget Name
drawItemSimple v =
      Brick.txt " "
  Brick.<+>
      Brick.txt (if v ^. (D.vWatched . D.vw) then "·" else "◇")
  Brick.<+>
      Brick.txt " "
  Brick.<+>
      Brick.padRight Brick.Max (Brick.txtWrap $ (D.runVideoTitle . D._vTitle) v)
  Brick.<+>
      Brick.txt " "
  Brick.<+>
      drawDate v

-- | Render an item of the video list as a two lines with the video title, its
-- publication date, and the title of the channel where it is published.
drawItemWithChannel :: D.Database -> D.Video -> Widget Name
drawItemWithChannel db v =
      Brick.txt " "
  Brick.<+>
      Brick.txt (if v ^. (D.vWatched . D.vw) then "·" else "◇")
  Brick.<+>
      Brick.txt " "
  Brick.<+>
      ( Brick.txtWrap ((D.runVideoTitle . D._vTitle) v)
      Brick.<=>
            Brick.padRight Brick.Max (drawChannelTitle db v)
  Brick.<+>
      Brick.txt " "
  Brick.<+> drawDate v)

-- | Render the video list as a Brick widget.
draw :: D.Database -> WidgetVideoList -> Widget Name
draw db (WidgetVideoList Watchlist brickWidget) =
    Brick.joinBorders $ Brick.borderWithLabel (Brick.txt " Watchlist ") $
        Brick.renderList (const (drawItemWithChannel db)) True brickWidget
draw db (WidgetVideoList (ChannelContent c) brickWidget) =
    Brick.joinBorders $ Brick.borderWithLabel (Brick.txt (" " <> (D.runChannelTitle . D.channelTitle $ c) <> " ")) $
        Brick.renderList (const drawItemSimple) True brickWidget

-- | Event handler for the video list (moving around).
handleEvent :: Vty.Event -> WidgetVideoList -> EventM Name WidgetVideoList
handleEvent vtyEvent (WidgetVideoList listType brickWidget) =
    WidgetVideoList listType <$>
        Brick.handleListEventVi Brick.handleListEvent vtyEvent brickWidget
