{-|
Module      : Tui.Splittable
Description : List instance of Splittable
Copyright   : (c) Florent Ch., 2021
License     : GPL-3.0-or-later

Splittable (Brick typeclass) instance for lists (to use in Brick list widgets).
-}

module Tui.Splittable where

import Brick.Widgets.List (Splittable, splitAt)

instance Splittable [] where
  splitAt n xs = (take n xs, drop n xs)
