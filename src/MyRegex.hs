module MyRegex where

import Data.Array((!))
import Data.Bits((.|.))
import Text.Regex.Base(RegexMaker(makeRegexOpts,makeRegex),defaultCompOpt,defaultExecOpt,RegexLike(matchAll,matchAllText),RegexContext(matchM),MatchText)
-- import Text.Regex.Posix(Regex)
import Text.Regex.PCRE(Regex)

mkRegex :: String -> Regex
-- mkRegex s = makeRegexOpts (compExtended .|. compNewline) defaultExecOpt s
mkRegex s = makeRegex s

subRegex :: Regex                          -- ^ Search pattern
         -> String                         -- ^ Input string
         -> String                         -- ^ Replacement text
         -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
  let compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . ((fst (m!x))++)
             else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = (makeRegex "\\\\(\\\\|[0-9]+)") :: Regex
        findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then pre ++ (compiled m "")
             else pre ++ (compiled m (go i' str' ms))
  in go 0 inp (matchAllText regexp inp)

