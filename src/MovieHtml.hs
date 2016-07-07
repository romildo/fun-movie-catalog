{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module MovieHtml where

import Prelude hiding (head,div,span,id)
import Control.Monad (unless, forM_)
import qualified Data.Array.IArray as A

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title,span,style,form)
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Internal (stringValue)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Movie
import CastGUI (parseCast)
import Rating (parseRatings)


movieToHtml (movie@(M{ident,info})) =
  table ! class_ "movietable" ! width "100%" $
    do tr $
         do td ! class_ "poster" ! rowspan "12" ! width "95" $
              img ! src (stringValue (info A.! Cover))
            td ! class_ "title" ! height "1*" $
              do b $ a ! class_ "MTITLE" ! href (stringValue (info A.! IMDB)) $
                   toHtml (info A.! Title)
                 "("
                 span ! class_ "MYEAR" $ toHtml (info A.! Year)
                 ")"
       tr ! class_ "moviedesc" $ td $
         b ! class_ "originaltitle" $ toHtml (info A.! OriginalTitle)
       tr ! class_ "moviedesc" $ td $
         do "Rating:"
            let rs = parseRatings (info A.! Rating)
            forM_ rs $ \(rater,rating,votes) ->
              do unless (null rater) (toHtml rater >> ":")
                 if rater == "IMDB"
                   then b ! class_ "MRATING" $ toHtml rating
                   else b $ toHtml rating
                 unless (null votes) ("(" >> toHtml votes >> ")")
       tr ! class_ "moviedesc" $ td $
         do "Runtime:" >> (b ! class_ "MRUNTIME" $ toHtml (info A.! Length)) >> " min"
            preEscapedToHtml ("&nbsp;&nbsp;"::String)
            "("
            i ! class_ "MGENRE" $ toHtml (info A.! Genres)
            ")"
       tr ! class_ "moviedesc" $ td $
         b $ toHtml (info A.! Certification)
       tr ! class_ "moviedesc" $ td $
         "Directors:" >> (b $ toHtml (info A.! Director))
       tr ! class_ "moviedesc" $ td $
         "Stars:" >> (b $ toHtml (info A.! Stars))
       tr ! class_ "moviedesc" $
         td ! class_ "plot" $
           toHtml (info A.! Synopsis)
       tr ! class_ "moviedesc" $
         td ! class_ "cast" $
           hideShow "Cast" ident (toHtml ("Cast"::String)) (castToHtml (info A.! Cast))
       tr ! class_ "moviedesc" $
         td ! class_ "parental_guide" $
           hideShow "ParentalGuide" ident (toHtml ("Parental Guide"::String)) (preEscapedToHtml (info A.! ParentalGuide))
       unless (null (info A.! Location)) $
         tr ! class_ "moviedesc" $ td $
           "Location:" >> (b $ toHtml (info A.! Location))
       tr ! class_ "moviemeta" $
         td ! height "1*" $
           do "File:"
              a ! href (stringValue (info A.! File)) $ toHtml (info A.! File)

castToHtml castText =
  table $ do thead (th "Name" >> th "Role")
             tbody $ forM_ (parseCast castText) $ \(n,r) ->
               tr $ td (toHtml n) >> td (toHtml r)


collectionToHtml Col{titles} =
  renderHtml $ docTypeHtml $
    do head $
         do title "Movie Catalog"
            script ! src "moviecat.js" ! type_ "text/javascript" $ ""
            link ! type_ "text/css" ! rel "alternate stylesheet" ! href "black.css" ! HA.title "black"
            link ! type_ "text/css" ! rel "alternate stylesheet" ! href "grey.css" ! HA.title "grey"
            link ! type_ "text/css" ! rel "alternate stylesheet" ! href "old.css" ! HA.title "old"
            link ! type_ "text/css" ! rel "stylesheet"           ! href "white.css" ! HA.title "white"
            style ! type_ "text/css" $
              do "td {vertical-align: top;}"
       body $
         do form ! HA.style "position: absolute; top: 2pt; right: 2pt;" ! name "ThemeForm" $
              do "Theme:"
                 select ! name "ThemeList" ! size "1" ! onchange "switchTheme(this.form)" $
                   do option ! value "black" $ "black"
                      option ! value "grey" $ "grey"
                      option ! value "old" $ "old"
                      option ! selected "white" ! value "white" $ "white"
            div ! class_ "sort-options" $
              do "Sort by:"
                 a ! id "SORT_TITLE" ! href "javascript:sort_title()" $ "Title"
                 a ! id "SORT_RATING" ! href "javascript:sort_rating()" $ "Rating"
                 a ! id "SORT_RUNTIME" ! href "javascript:sort_runtime()" $ "Runtime"
                 a ! id "SORT_YEAR" ! href "javascript:sort_year()" $ "Year"
                 small $ a ! id "SORT_DIRTIME" ! href "javascript:sort_dirtime()" $ "DirTime"       
            table ! id "MTABLE" ! class_ "movietable" $
              forM_ titles $ \movie -> tr $ td $ movieToHtml movie

hideShow name ident header body =
  -- do div ! HA.style "border: 1px solid blue; background-color: #99CCFF; padding: 5px; width: 100%;" $
  do div ! HA.style "width: 100%;" $
       do header
          a ! id (stringValue nameHeader) ! href (stringValue js) $ "show"
     div ! id (stringValue nameContent)
         -- ! HA.style "border: 1px solid black; background-color: #CCCCCC; display: none;padding: 5px; width: 100%;" $
         ! HA.style "border: 1px solid; display: none;padding: 5px; width: 100%;" $
       body
  where
    nameHeader = name ++  "Header" ++ show ident
    nameContent = name ++ "Content" ++ show ident
    js = "javascript:toggle2('" ++ nameContent ++ "','" ++ nameHeader ++ "');"

