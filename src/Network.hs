{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network
Description : Fetch info online about Youtube channels
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

This module provides functions to request and parse information about youtube
channels and their latest content mainly from their official RSS feeds.
-}

module Network
  ( extractChannelId
  , getFeed
  , parseTitle
  , parseVideos
  , NoChannelIdException(..)
  ) where

import qualified Database as D

import Data.List (find)
import Data.Maybe (catMaybes)

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text as Text

import qualified Network.HTTP.Simple as Http
import qualified Network.HTTP.Types.Status as Http

import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import Text.Feed.Types (Feed)
import qualified Text.Feed.Types as Feed

import qualified Text.HTML.TagSoup as Html
import qualified Text.HTML.TagSoup.Match as Html

import qualified Data.Time as Time

data NoChannelIdException = NoChannelIdException deriving (Show)

instance Exception NoChannelIdException

-- | Fetch the internal id of a channel from a the URL to the channel homepage.
-- WARN There used to be, in v0.1, a dedicated "channelId" metadata tag on
-- every webpage related to a channel (e.g. channel home or a video). It no
-- longer exists. Now the channel id is extracted from the "identifier"
-- metadata tag of the channel home. Videos have a similar tag but it contains
-- the video id. Therefore, using a video URL to add a channel no longer works.
extractChannelId :: (MonadThrow m, MonadIO m) => D.URL -> m D.ChannelId
extractChannelId url = do
  request <- Http.parseRequestThrow (Text.unpack url)
  response <- Http.httpLBS request
  let tags = Html.parseTags (Http.getResponseBody response)
      channelIdTag =
        find
          (Html.tagOpen (== "meta") (elem ("itemprop", "identifier")))
          tags
   in case Text.pack . Char8.unpack <$> (channelIdTag >>= maybeAttrib "content") of
        Nothing -> throwM NoChannelIdException
        Just cid -> return (D.ChannelId cid)
  where
    maybeAttrib :: Eq a => a -> Html.Tag a -> Maybe a
    maybeAttrib attribute (Html.TagOpen _ attributes) = lookup attribute attributes
    maybeAttrib _ _ = Nothing

-- | Request and parse the RSS feed of a channel.
getFeed :: D.ChannelId -> IO (Maybe Feed)
getFeed cid = do
  request <- Http.parseRequest (Text.unpack $ D.genChannelFeedUrl cid)
  response <- Http.httpLBS request
  return $
    if Http.statusIsSuccessful (Http.getResponseStatus response)
      then Feed.parseFeedSource (Http.getResponseBody response)
      else Nothing

-- | Get the channel title from the feed of a channel.
parseTitle :: Feed -> D.ChannelTitle
parseTitle = D.ChannelTitle . Feed.getFeedTitle

-- | Get the raw information about a video (an "item" in the feed of a channel).
parseItem :: Feed.Item -> Maybe D.VideoImport
parseItem i = do
  title <- Feed.getItemTitle i
  link <- Feed.getItemLink i
  datestring <- Feed.getItemPublishDateString i
  publicationTime <-
    Time.parseTimeM
      True
      Time.defaultTimeLocale
      "%Y-%m-%dT%H:%M:%S%Ez" -- Youtube feeds time format, e.g. 2020-09-21T08:41:34+00:00
      (Text.unpack datestring)
  vid <- D.extractVideoId link
  return (vid, D.VideoTitle title, D.VideoPublicationTime publicationTime)

-- | Get the raw information about the latest videos from the feed of a channel.
parseVideos :: Feed -> [D.VideoImport]
parseVideos f = catMaybes (parseItem <$> Feed.getFeedItems f)
