{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.AddChannel
Description : Widget to add a new channel
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Management of the widget to add a new channel from a user input URL.
-}

module Tui.AddChannel
  ( WidgetAddChannel
  , new
  , draw
  , content
  , handleEvent
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Brick.Types (Widget, EventM)
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.Edit as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Graphics.Vty as Vty

import Tui.Name (Name)
import qualified Tui.Name as Name


type WidgetAddChannel  = Brick.Editor Text Name

draw :: WidgetAddChannel -> Widget Name
draw editor =
  Brick.centerLayer $
  Brick.borderWithLabel (Brick.txt " URL to a page of the new channel ") $ 
  Brick.hLimit 80 $ Brick.renderEditor (Brick.txt . Text.unlines) True editor

new :: WidgetAddChannel
new = Brick.editorText Name.AddChannel (Just 1) Text.empty

content :: WidgetAddChannel -> Text
content = Text.concat . Brick.getEditContents

handleEvent :: Vty.Event -> WidgetAddChannel -> EventM Name WidgetAddChannel
handleEvent = Brick.handleEditorEvent
