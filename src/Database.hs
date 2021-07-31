{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Database
Description : Relational modeling of channels and videos
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Relational modeling of youtube channels and videos along with query and tool
functions. Relies on the ixset-typed library for multi indexed sets.
-}

module Database where

import Data.Data (Data, Typeable, Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import Lens.Micro.Platform

import Data.IxSet.Typed (IxSet)
import qualified Data.IxSet.Typed as IxSet

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe)

-- | Youtube channel in the database (a row in the channels table)
data Channel = Channel
  { _cId    :: ChannelId
  , _cTitle :: ChannelTitle
  } deriving (Show, Read, Eq, Ord, Data, Typeable)

-- | Youtube video in the database (a row in the videos table) including the
-- watch status and a reference to its youtube channel (1 channel, many
-- videos).
data Video = Video
  { _vId              :: VideoId
  , _vTitle           :: VideoTitle
  , _vChannelId       :: ChannelId
  , _vPublicationTime :: VideoPublicationTime
  , _vWatched         :: VideoWatched
  } deriving (Show, Read, Eq, Ord, Data, Typeable)

-- | Raw data about a video fetched from an RSS feed
type VideoImport = (VideoId, VideoTitle, VideoPublicationTime)

-- | Database of subscribed youtube channels and recent videos consisting of
-- tables: channels and videos.
data Database = Database
  { _mChannels :: Channels
  , _mVideos   :: Videos
  } deriving (Show, Read)

newtype ChannelId            = ChannelId {runChannelId :: Text}                     deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype ChannelTitle         = ChannelTitle {runChannelTitle :: Text}               deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype VideoId              = VideoId {runVideoId :: Text}                         deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype VideoTitle           = VideoTitle {runVideoTitle :: Text}                   deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype VideoPublicationTime = VideoPublicationTime {runPublicationTime :: UTCTime} deriving (Show, Read, Eq, Ord, Data, Typeable)
newtype VideoWatched         = VideoWatched {_vw :: Bool}                           deriving (Show, Read, Eq, Ord, Data, Typeable)

type VideoIxs = '[VideoId, ChannelId, VideoPublicationTime, VideoWatched]
type Videos  = IxSet VideoIxs Video

type ChannelIxs = '[ChannelId, ChannelTitle]
type Channels  = IxSet ChannelIxs Channel

instance IxSet.Indexable VideoIxs Video where
  indices =
    IxSet.ixList
    (IxSet.ixGen (Proxy :: Proxy VideoId))
    (IxSet.ixGen (Proxy :: Proxy ChannelId))
    (IxSet.ixGen (Proxy :: Proxy VideoPublicationTime))
    (IxSet.ixGen (Proxy :: Proxy VideoWatched))

instance IxSet.Indexable ChannelIxs Channel where
  indices =
    IxSet.ixList
    (IxSet.ixGen (Proxy :: Proxy ChannelId))
    (IxSet.ixGen (Proxy :: Proxy ChannelTitle))

makeLenses ''Channel
makeLenses ''Video
makeLenses ''Database
makeLenses ''VideoWatched

type URL = Text

------------------

empty :: Database
empty = Database IxSet.empty IxSet.empty

channelIds :: Database -> Set ChannelId
channelIds = Set.map _cId . IxSet.toSet . _mChannels

channelId :: Channel -> ChannelId
channelId = _cId

channelTitle :: Channel -> ChannelTitle
channelTitle = _cTitle

videoId :: Video -> VideoId
videoId = _vId

publicationTime :: Video -> VideoPublicationTime
publicationTime = _vPublicationTime

importChannel :: Channel -> Database -> Database
importChannel c = mChannels %~ IxSet.updateIx (c ^. cId) c

removeChannel :: ChannelId -> Database -> Database
removeChannel cid db =
  (mVideos %~ \x -> foldr IxSet.delete x (channelContent cid db))
  . (mChannels %~ IxSet.deleteIx cid)
  $ db

lookupChannel :: ChannelId -> Database -> Maybe Channel
lookupChannel cid = IxSet.getOne . (IxSet.@= cid) . _mChannels

lookupVideo :: VideoId -> Database -> Maybe Video
lookupVideo vid = IxSet.getOne . (IxSet.@= vid) . _mVideos

channelContent :: ChannelId -> Database -> Videos
channelContent cid = (IxSet.@= cid) . _mVideos

lookupChannelContent :: ChannelId -> Database -> Set Video
lookupChannelContent cid = IxSet.toSet . channelContent cid

lookupNotWatched :: Database -> Set Video
lookupNotWatched = IxSet.toSet . (IxSet.@= VideoWatched False) . _mVideos

lookupNotWatchedByDate :: Database -> [Video]
lookupNotWatchedByDate =
  IxSet.toDescList (Proxy :: Proxy VideoPublicationTime)
  . (IxSet.@= VideoWatched False)
  . _mVideos

lookupNotWatchedOnChannel :: ChannelId -> Database -> Set Video
lookupNotWatchedOnChannel cid =
  IxSet.toSet
  . (IxSet.@= VideoWatched False)
  . channelContent cid

lookupLatestVideoOnChannel :: ChannelId -> Database -> Maybe Video
lookupLatestVideoOnChannel cid =
  listToMaybe
  . IxSet.toDescList (Proxy :: Proxy VideoPublicationTime)
  . channelContent cid

lookupLatestNotWatchedVideoOnChannel :: ChannelId -> Database -> Maybe Video
lookupLatestNotWatchedVideoOnChannel cid =
  listToMaybe
  . IxSet.toDescList (Proxy :: Proxy VideoPublicationTime)
  . IxSet.getEQ (VideoWatched False)
  . channelContent cid

lookupLatestContentOnChannel :: ChannelId -> Database -> Set Video
lookupLatestContentOnChannel cid = IxSet.toSet . channelContent cid

lookupLatestContentOnChannelByDate :: ChannelId -> Database -> [Video]
lookupLatestContentOnChannelByDate cid =
  IxSet.toDescList (Proxy :: Proxy VideoPublicationTime)
  . channelContent cid

-- | Apply a function to the element of the set corresponding to a primary key
updateIxFun :: (IxSet.Indexable ixs a, IxSet.IsIndexOf ix ixs) => (a -> a) -> ix -> IxSet ixs a -> IxSet ixs a
updateIxFun f primaryKey set =
  case IxSet.getOne (set IxSet.@= primaryKey) of
    Nothing -> set
    Just x -> IxSet.updateIx primaryKey (f x) set

-- | Apply a function to the elements of the set corresponding to a collection of primary keys
updateIxFunFold :: (IxSet.Indexable ixs a, IxSet.IsIndexOf ix ixs, Foldable f) => f ix -> (a -> a) -> IxSet ixs a -> IxSet ixs a
updateIxFunFold pks f set = foldr (updateIxFun f) set pks

-- | Delete elements in a set corresponding to primary keys in a given collection
deleteIxMany :: (IxSet.Indexable ixs a, IxSet.IsIndexOf ix ixs, Foldable f) => f ix -> IxSet ixs a -> IxSet ixs a
deleteIxMany pks set = foldr IxSet.deleteIx set pks

-- | Toggle watch status of a video if it exists in the database
toggleWatched :: VideoId -> Database -> Database
toggleWatched vid = mVideos %~ updateIxFun (vWatched . vw %~ not) vid

-- | Mark a collection of videos watched
setWatched :: Foldable f => f VideoId -> Database -> Database
setWatched vids = mVideos %~ updateIxFunFold vids (vWatched . vw .~ True)

-- | Import a video in the videos table and set its watch status. If it already
-- exists, it updates its title.
importVideo :: VideoWatched -> ChannelId -> VideoImport -> Videos -> Videos
importVideo w cid (vid, title, publicationTime) videos =
  let video =
        case IxSet.getOne (videos IxSet.@= vid) of
          Nothing ->
            Video
              { _vId = vid,
                _vTitle = title,
                _vChannelId = cid,
                _vPublicationTime = publicationTime,
                _vWatched = w
              }
          Just x -> x & vTitle .~ title
   in IxSet.updateIx vid video videos

-- | Integrate new information about a channel (the channel and its latest
-- videos) into the database. Some videos are no longer mentioned in the
-- channel feed. They are deleted from the local database to avoid accumulation
-- (design choice). New videos are imported into the database and set to a
-- given initial watch status. Implementation based on video ids. Could be
-- implemented by comparing publication dates.
updateChannelWithWatchStatus ::  VideoWatched -> Channel -> Set VideoImport -> Database -> Database
updateChannelWithWatchStatus w c vs m =
  let storedVideoIds = Set.map _vId $ lookupChannelContent (c ^. cId) m
      expiredVideoIds = Set.difference storedVideoIds (Set.map (\(x, _, _) -> x) vs)
      cleanup = mVideos %~ deleteIxMany expiredVideoIds
      updateChannel = importChannel c
      updateLatestVideos = mVideos %~ \x -> foldr (importVideo w (c ^. cId)) x vs
   in cleanup . updateLatestVideos . updateChannel $ m

-- | Integrate new information about a channel into the database with new
-- videos marked not watched.
updateChannel :: Channel -> Set VideoImport -> Database -> Database
updateChannel = updateChannelWithWatchStatus (VideoWatched False)

-- | Integrate new information about a channel into the database but the new
-- videos will be marked as already watched.
updateChannelSilently :: Channel -> Set VideoImport -> Database -> Database
updateChannelSilently = updateChannelWithWatchStatus (VideoWatched True)

genChannelUrl :: ChannelId -> URL
genChannelUrl (ChannelId txt) = "https://www.youtube.com/channel/" <> txt

genVideoUrl :: VideoId -> URL
genVideoUrl (VideoId txt) = "https://www.youtube.com/watch?v=" <> txt

extractVideoId :: URL -> Maybe VideoId
extractVideoId = fmap VideoId . Text.stripPrefix "https://www.youtube.com/watch?v="

genChannelFeedUrl :: ChannelId -> URL
genChannelFeedUrl (ChannelId id) = "https://www.youtube.com/feeds/videos.xml?channel_id=" <> id
