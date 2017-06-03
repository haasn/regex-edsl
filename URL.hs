{-# LANGUAGE OverloadedStrings, DataKinds #-}
import Text.Regex.EDSL

-- Matches any valid URI, plus some variants of "naked" domains
url :: Regex
url = wordBoundary :* (uri :| uriLike)
    where uriLike = "www." :* validStr
                 :| validStr :* foldr1 (:|) domains :* hardBoundary

          hardBoundary = lookNot Ahead $ charSpecial "S"
          domains = [ ".com", ".net", ".org", ".de", ".co.uk" ]

-- Matches anything that looks like a valid URI
uri :: Regex
uri = scheme :* validStr
    where scheme = foldr1 (:|) protos :* ":"
          protos = [ "http" :* Opt "s", "ftp", "file", "mailto", "magnet" ]

-- Risky chars could be URL separators instead of part of the URL, so try
-- not matching them if they're the last thing in the URL
safeChars, riskyChars :: CharGroup Positive
safeChars  = wordChar <> "-@;/?:&=%$+*~#"
riskyChars = safeChars <> ".!',"

-- This matches any of safeChars, unicode numerals/symbols, or unicode letters
-- followed by unicode combining diacritics
safeLetter :: Regex
safeLetter = charClass (safeChars <> groupSpecial "p{N}" <> groupSpecial "p{S}")
          :| charSpecial "p{L}" :* Many (charSpecial "p{M}")

-- This matches any safe letter, plus risky punctuation
riskyLetter :: Regex
riskyLetter = safeLetter :| charClass riskyChars
           :| lookNot Ahead "()[]" :* charSpecial "p{P}"

-- Valid character sequence that can be part of a URL
validStr :: Regex
validStr = Many riskyLetter :* Some validGroup
    where validGroup = "(" :* Many riskyLetter :* ")"
                    :| "[" :* Many riskyLetter :* "]"
                    :| safeLetter
