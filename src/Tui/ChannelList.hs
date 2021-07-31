{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.ChannelList
Description : Channel list widget
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Management of the channel list widget.
-}

module Tui.ChannelList
  ( WidgetChannelList
  , new
  , update
  , draw
  , selectedChannel
  , handleEvent
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

import Brick.Types (Widget, EventM)
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.List as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Graphics.Vty as Vty

import qualified Database as D

import Tui.Splittable
import Tui.Name (Name)
import qualified Tui.Name as Name


-- | An item of the channel list which contains additional data, besides the
-- channel itself, involved in ordering and displaying the channel list.
data Item = Item
  { _channel :: D.Channel -- ^ The channel
  , _unseenCount :: Int -- ^ Number of unseen videos to display in the interface
  , _latestNotWatchedVideo :: Maybe D.Video -- ^ Latest video to order by date
  , _updating :: Bool -- ^ Whether new info about the channel is being fetched online
  }

instance Eq Item where
  (Item c _ _ _) == (Item c' _ _ _) = D.channelId c == D.channelId c'

-- | Order channels by latest video publication date
instance Ord Item where
  (Item _ _ v _) <= (Item _ _ v' _) =
    (D.publicationTime <$> v) >= (D.publicationTime <$> v')

-- | Widget for channel lists
type WidgetChannelList = Brick.GenericList Name [] Item

-- | Build a list item corresponding to a channel id by retrieving the
-- necessary info in a database of channels and videos. A set of channel ids
-- being updated is used to determine if the updating flag is to be set.
item :: D.Database -> Set D.ChannelId -> D.ChannelId -> Maybe Item
item db updatingChannels cid = do
  c <- D.lookupChannel cid db
  return $
    Item
    c
    (Set.size (D.lookupNotWatchedOnChannel cid db))
    (D.lookupLatestNotWatchedVideoOnChannel cid db)
    (Set.member cid updatingChannels)

-- | Generate a new brick widget associated to the channels in the database. A
-- set of ids of channels that are currently being updated is used to display a
-- specific update icon. Ids are silently ignored if not found in the database.
new :: D.Database -> Set D.ChannelId -> WidgetChannelList
new db updatingChannels =
  Brick.list
  Name.ChannelList
  (sort $ mapMaybe (item db updatingChannels) (Set.toList $ D.channelIds db))
  1

-- | Update an existing channel list widget. That is generate a new one with
-- updated data and move the selection to the previously selected item. A set
-- of ids of channels that are currently being updated is used to display a
-- specific update icon. Ids are silently ignored if not found in the database.
update :: D.Database -> Set D.ChannelId -> WidgetChannelList -> WidgetChannelList
update db updatingChannels list =
  case snd <$> Brick.listSelectedElement list of
    Nothing -> new db updatingChannels
    Just selected -> Brick.listMoveToElement selected (new db updatingChannels)

-- | Appearance of a channel list item using Brick widgets. It consists of a
-- line with the number of unseen videos, an icon for new content and updating
-- status, and the title of the channel.
drawItem :: Item -> Widget Name
drawItem (Item c n v u) =
  Brick.txt
  ( if n > 0 then (if n < 10 then " " else "")
  <> (Text.pack . show $ n) else "  ")
  Brick.<+>
  Brick.txt " "
  Brick.<+>
  Brick.txt (if u then "◈" else if n > 0 then "◇" else "·")
  Brick.<+>
  Brick.txt " "
  Brick.<+>
  Brick.txtWrap (D.runChannelTitle $ D.channelTitle c)

-- | Render the channel list as a Brick widget.
draw :: WidgetChannelList -> Widget Name
draw list =
  Brick.borderWithLabel (Brick.txt " Channels ") $
    Brick.renderList
    (\_ i -> drawItem i)
    True
    list

-- | The select channel in a channel list.
selectedChannel :: WidgetChannelList -> Maybe D.ChannelId
selectedChannel list = D.channelId . _channel . snd <$> Brick.listSelectedElement list

-- | Event handler for the channel list (moving around).
handleEvent :: Vty.Event -> WidgetChannelList -> EventM Name WidgetChannelList
handleEvent = Brick.handleListEventVi Brick.handleListEvent
