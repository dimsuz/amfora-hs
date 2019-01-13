{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                 (void)
import qualified Control.Concurrent.Async as Async

import qualified GI.Gtk as Gtk

import           GI.Gtk                        (Label (..), Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial | Greeting Text

data Event = Greet Text | Closed

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ case s of
        Initial      -> widget Label [#label := "Nothing here yet."]
        Greeting who -> widget Label [#label := who]

update' :: State -> Event -> Transition State Event
update' _ (Greet who) = Transition (Greeting who) (return Nothing)
update' _ Closed      = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [greetings]
  , initialState = Initial
  }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> (Greet ("Hello, " <> n)))
      & Pipes.each
      & (>-> Pipes.delay 1.0)

-- Manual running with runLoop, has the same problems...

-- main :: IO ()
-- main = do
--   Gtk.init Nothing
--   a <- Async.async (do
--     runLoop (App
--       { view         = view'
--       , update       = update'
--       , inputs       = [greetings]
--       , initialState = Initial
--       })
--     Gtk.mainQuit)
--   Gtk.main
--   where
--     greetings =
--       cycle ["Joe", "Mike"]
--         & map (\n -> (Greet ("Hello, " <> n)))
--         & Pipes.each
--         & (>-> Pipes.delay 1.0)
