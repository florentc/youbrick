{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Tui.KeybindingsIndicator
Description : Widget to display keybindings
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Passive widget to display currently available keybindings.
-}

module Tui.KeybindingsIndicator
  ( draw
  ) where

import Data.Text (Text)

import Brick.Types (Widget)
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Center as Brick

import Tui.State
import Tui.Name (Name)

-- | A text representation of the key and a text description of its function.
type Keybinding = (Text, Text)

-- | Keybindings for each activity in the application.
activityKeybindings :: Activity -> [Keybinding]
activityKeybindings ConsultChannelList =
  [ ("a", "Add")
  , ("d", "Delete")
  , ("r", "Refresh")
  , ("l", "Watchlist")
  , ("w", "Set watched")
  , ("Enter", "Open")
  , ("b", "Browser")
  , ("Esc", "Exit") ]
activityKeybindings AddChannel =
  [ ("Enter", "Submit")
  , ("Esc", "Cancel") ]
activityKeybindings (ConsultVideoList _) =
  [ ("Enter", "Watch")
  , ("w", "Toggle watched")
  , ("b", "Browser")
  , ("Esc", "Channel list") ]

-- | Render a keybinding indicator.
drawKeybinding :: Keybinding -> Widget Name
drawKeybinding (key, description) =
  Brick.padLeftRight 1 $
    Brick.txt ("[" <> key <> "]") Brick.<=> Brick.txt description

-- | Render the keybindings indicator as a ribbon.
draw :: Activity -> Widget Name
draw = Brick.hCenter . Brick.hBox . map drawKeybinding . activityKeybindings
