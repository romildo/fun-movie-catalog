module MovieFormType where

import Data.Array.IArray (Array)
import Graphics.UI.Gtk.WebKit.WebView (WebView)

import Component (AnyComponent)
import Movie (Info, CommentInfo, DiscInfo)


data MovieForm =
  MovieForm
  {
    mfIdent   :: AnyComponent
  , mfInfo    :: [(Info, AnyComponent)]
  , mfComment :: ([Array CommentInfo String] -> IO (), IO [Array CommentInfo String], ())
  , mfDisc    :: ([Array DiscInfo String] -> IO (), IO [Array DiscInfo String])
  , mfMovie   :: WebView
  }

