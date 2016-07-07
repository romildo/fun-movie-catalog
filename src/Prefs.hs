{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Prefs where

import Data.Maybe (catMaybes)
import Data.List (stripPrefix, (\\))
import Data.List.Utils (split, join)
import Data.IORef (readIORef, writeIORef)
import qualified Data.ConfigFile as CF
import qualified Data.Map as Map
import Control.Monad (foldM, when)
import Control.Applicative ((<$>))
import System.Directory (getAppUserDataDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (combine, takeDirectory)
import Graphics.UI.Gtk hiding (Release)
import Network.Browser (Cookie)

import Movie (Info(..), infoMap)
import SiteConfig (SiteConfig(siteName))
import Component (mkWA, myBoxPackStart, packInTableD, errorMsg)
import Util (trim)

data Prefs = Prefs
  { player :: String
  , browser :: String
  , cookies :: [(String,Cookie)]
  , infoSearch :: [(Info,InfoPref)]
  }
  deriving (Show)

defaultPrefs =
  Prefs
  { player = "mplayer"
  , browser = "firefox"
  , cookies = []
  , infoSearch = [ (AlternateTitle,IPCombine)
                 , (Genres,IPCombine)
                 , (Release,IPCombine)
                 , (Rating,IPCombine)
                 , (Score,IPCombine)
                 , (Certification,IPCombine)
                 -- , (ParentalGuide,IPCombine)
                 , (Keywords,IPCombine)
                 -- , (Cast,IPCombine)
                 , (Cast,IPSelected ["EPipoca","IMDB"])
                 , (Awards,IPCombine)
                 , (Curiosities,IPCombine)
                 , (Notes,IPCombine)
                 , (Cover,IPSelected ["Interfilmes","AdoroCinema"])
                 ]
  }

data InfoPref
  = IPFirst
  | IPLast
  | IPCombine
  | IPSelected [String]
  deriving (Show)


prefsFilePath =
  do dir <- getAppUserDataDirectory "movie-catalog"
     return (combine dir "preferences")

readPreferences parent sites =
  do filePath <- prefsFilePath
     exists <- doesFileExist filePath
     if exists
     then do val <- CF.readfile CF.emptyCP filePath
             case val of
               Left msg ->
                 do errorMsg parent (show msg)
                    return defaultPrefs
               Right cp ->
                 return $ Prefs
                      { player = either (const (player defaultPrefs)) id (CF.get cp "DEFAULT" "player")
                      , browser = either (const (browser defaultPrefs)) id (CF.get cp "DEFAULT" "browser")
                      , cookies = either (const []) (map (\(a,b) -> (a, read b))) (CF.items cp "COOKIES")
                      , infoSearch = let f xs = flip map xs $ \(a,b) ->
                                                  case Map.lookup a infoMap of
                                                    Just i -> (i,) <$> readInfoPref sites b
                                                    Nothing -> Nothing
                                     in catMaybes (either (const []) f (CF.items cp "INFO FROM SITES"))
                      }
     else return defaultPrefs

savePreferences parent Prefs{player,browser,cookies,infoSearch} =
  do let cp = do let cp = CF.emptyCP
                 cp <- CF.set cp "DEFAULT" "player" player
                 cp <- CF.set cp "DEFAULT" "browser" browser
                 cp <- CF.add_section cp "COOKIES"
                 foldM (\cp (i,x) -> CF.set cp "COOKIES" i (show x)) cp cookies
                 cp <- CF.add_section cp "INFO FROM SITES"
                 foldM (\cp (i,x) -> CF.set cp "INFO FROM SITES" (show i) (showInfoPref x)) cp infoSearch
     filePath <- prefsFilePath
     createDirectoryIfMissing True (takeDirectory filePath)
     either (const (return ())) (writeFile filePath . CF.to_string) cp


readInfoPref _ "first" = Just IPFirst
readInfoPref _ "last"  = Just IPLast
readInfoPref _ "combine"   = Just IPCombine
readInfoPref sites str = case stripPrefix "selected:" str of
                           Just str' -> let siteNames = map siteName sites
                                            names = filter (flip elem siteNames) (split "," str')
                                        in Just $ IPSelected $  names ++ (siteNames \\ names)
                           Nothing -> Nothing

showInfoPref IPFirst = "first"
showInfoPref IPLast  = "last"
showInfoPref IPCombine = "combine"
showInfoPref (IPSelected xs) = "selected:" ++ join "," xs




prefsSetup parent prefsRef =
  do d <- dialogNew
     set d [ windowTitle := "Movie Collection Manager Preferences"
           , windowTransientFor := parent
           ]
     dialogAddButton d stockCancel ResponseReject
     ok <- dialogAddButton d stockApply ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     --
     prefs@Prefs{player,browser} <- readIORef prefsRef
     t <- mkWA (tableNew 0 2 False) (myBoxPackStart u PackNatural 0) [containerBorderWidth := 5]
     playerEntry <- mkWA entryNew (packInTableD t 0 0 2 "_Player") [entryText := player]
     browserEntry <- mkWA entryNew (packInTableD t 1 0 2 "_Browser") [entryText := browser]
     --
     widgetShowAll d
     resp <- dialogRun d
     when (resp == ResponseAccept) $
       do player <- trim <$> entryGetText playerEntry
          browser <- trim <$> entryGetText browserEntry
          let prefs' = prefs{player,browser}
          writeIORef prefsRef prefs'
          savePreferences parent prefs'
     widgetDestroy d
