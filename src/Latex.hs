{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Latex where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Ix (range)
import Data.Array.IArray (assocs)
import Data.List (intersperse, intercalate)
import Text.Printf (printf)

import Util (trim, replace, matchREs, splitRegex)
import Util.Regex (subRegex)
import Movie (infoBounds, Info(..), translateInfo, Movie(info))
import Rating (parseRatings)

field i (j,x) = i == j

look i xs = fromMaybe "" (lookup i xs)

loop x (f:fs) = loop (f x) fs
loop x [] = x

f movie i text = replace ("__" ++ show i ++ "__") (esc (look i movie)) text


esc text =
  loop text $ map (\(a,b) -> replace a b)
                [ ("\\", "\\textbackslash")
                , ("~", "\\textasciitilde")
                , ("$", "\\$")
                , ("#", "\\#")
                , ("%", "\\%")
                , ("&", "\\&")
                , ("_", "\\_")
                , ("^", "\\textasciicircum")
                , ("{", "\\{")
                , ("}", "\\}")
                ]

latexA =
  unlines
  [ "\\documentclass[a4paper]{article}"
  , "\\usepackage[hmargin={10mm,10mm},vmargin={20mm,5mm},nofoot]{geometry}"
  , "\\usepackage[T1]{fontenc}"
  , "\\usepackage[utf8]{inputenc}"
  , "\\usepackage[brazil]{babel}"
  , "\\usepackage{pslatex}"
  , "\\usepackage{indentfirst}"
  , "\\usepackage{multicol}"
  , "\\usepackage{tabularx}"
  , "\\renewcommand{\\tabularxcolumn}[1]{m{#1}}"
  , "\\usepackage[usenames,dvipsnames]{color}"
  , "\\usepackage[breaklinks=true]{hyperref}"
  , "\\usepackage{graphicx}"
  , "\\usepackage{grffile}"
  , "\\usepackage{stmaryrd}"
  , "\\usepackage{ifthen}"
  , "\\usepackage{enumitem}"
  , "\\setitemize{leftmargin=*}"
  , "\\setlist{itemsep=0.01em}"
  , "\\setdescription{font=\\slshape\\mdseries}"
  , "%\\setitemize[1]{label=$\boxempty$}"
  , "\\usepackage{sectsty}"
  , "\\usepackage[normalem]{ulem}"
  , "\\usepackage{lastpage}"
  , "\\usepackage{MnSymbol}"
  , "\\usepackage{fancyhdr}"
  , "\\fancyfoot{}"
  , "\\renewcommand{\\headrulewidth}{0.4pt}"
  , "\\setlength{\\headsep}{12pt}"
  , "\\pagestyle{fancy}"
  , "\\fancypagestyle{empty}{%%"
  , "\\fancyhf{}"
  , "   \\renewcommand{\\headheight}{0pt}"
  , "   \\renewcommand{\\headsep}{0pt}"
  , "   \\renewcommand{\\footskip}{0pt}"
  , "   \\renewcommand{\\headrulewidth}{0pt}"
  , "   \\renewcommand{\\footrulewidth}{0pt}"
  , "}"
  , "\\sectionfont{\\large\\sectionrule{0pt}{0pt}{-.7ex}{.2pt}}"
  , ""
  , ""
  , "\\begin{document}"
  , ""
  ]

latexZ =
  unlines
  [ ""
  , "\\end{document}"
  , ""
  ]

latexMovie movie =
  loop template (map (f fields) (range infoBounds))
  where
    fields = assocs (info movie)

    whenField i f =
      case look i fields of
        "" -> ""
        x -> unlines (f x)

    template =
      unlines
      [ "\\newpage"
      , ""
      , "\\lhead{\\textbf{__Title__} (\\textit{__OriginalTitle__}, __Year__)}"
      , "\\chead{}"
      , "\\rhead{\\thepage\\ de \\pageref{LastPage}}"
      , ""
      , "\\thispagestyle{empty}"
      , ""
      , "\\noindent\\framebox[\\linewidth]{"
      , "  \\begin{tabularx}{\\linewidth}{>{\\raggedright}l>{\\raggedleft}X}"
      , "    \\Large\\textbf{__Title__} &"
      , "    \\textbf{\\textsl{__OriginalTitle__}} \\par __Country__. \\textbf{__Year__} \\par __Genres__ \\par \\textcolor{blue}{__Certification__} \\par __Location__"
      , "  \\end{tabularx}"
      , "}"
      , ""
      , "\\setlength{\\columnseprule}{.4pt}"
      , "\\begin{multicols}{3}"
      , ""
      , whenField Cover $ \x -> ["\\begin{center}", "  \\includegraphics[width=\\linewidth]{" ++ x ++ "}", "\\end{center}" ]
      , ""
      , whenField Synopsis $ \x -> [ "\\section*{Sinopse}", "", esc x, "" ]
      , whenField Rating $ \x -> [ "\\section*{Rating}", "", latexRatings x, "" ]
      , whenField Cast $ \x -> [ "\\section*{Elenco}", "", latexCast x, "" ]
      , ""
      , "\\section*{Dados do filme}"
      , ""
      , description $ catMaybes $ map (\(i,f) -> fmap ((translateInfo i,) . f) (lookup i fields)) $
        [ (OriginalTitle    , id)
        , (AlternateTitle   , latexLines . splitRegex "\\s*/\\s*" . trim)
        , (Year             , id)
        , (Release          , id)
        , (Country          , id)
        , (Director         , id)
        , (Stars            , id)
        , (Length           , (++" minutos"))
        , (Genres           , id)
        , (Certification    , id)
        , (Type             , id)
        , (Theme            , ("\\textcolor{blue}{"++) . (++"}"))
        , (Contains         , ("\\textcolor{blue}{"++) . (++"}"))
        , (Keywords         , id)
        , (Studio           , id)
        , (Distributor      , id)
        , (Screenwriter     , id)
        , (Producer         , id)
        , (Music            , id)
        , (Photography      , id)
        , (ProductionDesign , id)
        , (ArtDirection     , id)
        , (Figurino         , id)
        , (Editor           , id)
        , (SpecialEffects   , id)
        , (Site             , ("\\url{"++) . (++"}"))
        , (IMDB             , ("\\url{"++) . (++"}"))
        ]
      , ""
      , "\\section*{Dados técnicos}"
      , ""
      , description $ catMaybes $ map (\(i,f) -> fmap ((translateInfo i,) . f) (lookup i fields)) $
        [ (Audios        , audiosLine)
        , (Subtitles     , subtitlesLine)
        , (Screen        , id)
        , (Format        , id)
        , (InclusionDate , id)
        ]
      , ""
      , sectionList fields Notes
      , ""
      , sectionList fields Curiosities
      , ""
      , sectionList fields Awards
      , ""
      , "\\end{multicols}"
      , ""
      ]


latex movie =
  unlines [latexA, latexMovie movie, latexZ]


sectionList movie i
  | null x = ""
  | otherwise = printf "\\section*{%s}\n\n%s\n" (translateInfo i) (makeLatexList x)
  where
    x = fromMaybe "" (lookup i movie)


makeLatexList text =
  unlines $
  [ "\\begin{itemize}" ] ++
  map id (splitRegex text "\\s*\n(\\s*[*-])?\\s*") ++
  [ "\\end{itemize}" ]


latexCast text =
  unlines $ intersperse "\n" $
    [ "\\begin{flushleft}" ] ++
    map f (splitCast text) ++
    [ "\\end{flushleft}\n" ]
  where
    f ("vozes:","") = "  \\textbf{Vozes:}"
    f (x,"")        = "  \\textsl{" ++ esc x ++ "}"
    f (x,y)         = "  \\textsl{" ++ esc x ++ "} (" ++ esc y ++ ")"

splitCast text =
  map f (splitRegex "\\s*(\n|;|,| e )\\s*" text)
  where
    f = head . matchREs [ ( "(.+)\\s*\\(\\s*(.*)\\s*\\)", \ [_,x,y] -> (x,y)  )
                        , ( "(.+)\\s*\\.{3,}\\s*(.*)"   , \ [_,x,y] -> (x,y)  )
                        , ( "(.+)"                      , \ [_,x]   -> (x,"") )
                        ]

latexRatings text =
  intercalate "\n" $
    [ "\\begin{flushleft}" ] ++
    [ intercalate "\\newline\n" (map f (parseRatings text)) ] ++
    [ "\\end{flushleft}\n" ]
  where
    f (rater,rating,votes) =
      "  " ++
      (if null rater then "" else "\\textsl{" ++ esc rater ++ "}: ") ++
      "\\textbf{" ++ esc rating ++ "}" ++
      (if null votes then "" else " (" ++ esc votes ++ " votes)")


description items =
  unlines $
  [ "\\begin{description}" ] ++
  map f items ++
  [ "\\end{description}" ]
  where
    f (x,y) = printf "  \\item[%s:] %s" x y


latexLines (x:xs) =
  unlines $
    [ "\\mbox{}"
    , ""
    , "\\begin{itemize}[topsep=0em,partopsep=0mm,parsep=0em,itemsep=0em]"
    , printf "  \\item %s" (esc x)
    ] ++
    map (printf "  \\item %s" . esc) xs ++
    [ "\\end{itemize}" ]
latexLines [] = []


audiosLine text =
  concat $ intersperse "," $ map f (splitRegex "\\s*,\\s*" text)
  where
    f audio = case splitRegex ":" audio of
                [] -> ""
                x:[] -> x
                x:xs -> printf "%s (%s)" x (concat (intersperse ", " xs))


subtitlesLine text =
  subRegex text "\\s*,\\s*" ", "

