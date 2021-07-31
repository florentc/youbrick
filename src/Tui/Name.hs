{-|
Module      : Tui.Name
Description : Names of the TUI widgets
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Names of the widgets in the TUI.
-}

module Tui.Name where

data Name
  = ChannelList
  | VideoList
  | AddChannel
  | OpenWith
  deriving (Eq, Ord, Show)
