{-# LANGUAGE FlexibleContexts #-}

module Util.Regex where

-- see http://hpaste.org/41056/regex_replace

import Text.Regex.PCRE (Regex)

import Text.Regex.Base (RegexMaker(makeRegexM), RegexLike(matchOnceText), RegexContext(match))
import Data.Array (bounds, (!))
import Data.Maybe (fromMaybe)

subRegexM str ptn repl =
  do re <- makeRegexM ptn
     subRegexReM str (re::Regex) repl

subRegexReM str re repl =
  case matchOnceText re str of
    Nothing -> return str
    Just (before,match,after) ->
      do repl' <- backrefM "" repl match
         after' <- subRegexReM after re repl
         return (before ++ repl' ++ after')

backrefM pref [] _ = return pref
backrefM pref (a:xs) match
  | a == '\\' = case xs of
                  b:ys | b == '\\' -> backrefM (pref ++ "\\") ys match
                       | b >= '0' && b <= '9' -> let i = read [b]
                                                     (_,n) = bounds match
                                                 in if i <= n
                                                    then let (text,_) = match!i
                                                         in backrefM (pref ++ text) ys match
                                                    else fail ['\\',b]
                       | otherwise -> fail ['\\',b]
                  [] -> fail "\\"
  | otherwise = backrefM (pref ++ [a]) xs match


subRegex str ptn repl = fromMaybe str (subRegexM str ptn repl)
