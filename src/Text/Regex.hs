{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) Chris Kuklewicz 2006, (c) shelarcy 2012, derived from (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- Regular expression matching.  Uses the POSIX regular expression
-- interface in "Text.Regex.TDFA".
--
---------------------------------------------------------------------------

--
-- Modified by Chris Kuklewicz to be a thin layer over the regex-posix
-- package, and moved into a regex-compat package.
--
-- Modified by Adam Gundry to contain only the functions needed for api-tools.
--
module Text.Regex (
    -- * Regular expressions
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll
  ) where

import Text.Regex.Base(RegexMaker(makeRegexOpts),defaultCompOpt,defaultExecOpt,RegexContext(matchM))
import Text.Regex.TDFA(Regex,caseSensitive,multiline,newSyntax)

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = makeRegexOpts opt defaultExecOpt s
  where opt = defaultCompOpt { newSyntax = True, multiline = True }

-- | Makes a regular expression, where the multi-line and
-- case-sensitive options can be changed from the default settings.
mkRegexWithOpts
   :: String  -- ^ The regular expression to compile
   -> Bool    -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and
              -- end of individual lines respectively, and @\'.\'@ does /not/
              -- match the newline character.
   -> Bool    -- ^ 'True' @\<=>@ matching is case-sensitive
   -> Regex   -- ^ Returns: the compiled regular expression

mkRegexWithOpts s single_line case_sensitive
  = let opt = defaultCompOpt
                { multiline    = (if single_line then True else False)
                , caseSensitive = (if case_sensitive then True else False)
                , newSyntax     = True }
    in makeRegexOpts opt defaultExecOpt s

-- | Match a regular expression against a string
matchRegex
   :: Regex     -- ^ The regular expression
   -> String    -- ^ The string to match against
   -> Maybe [String] -- ^ Returns: @'Just' strs@ if the match succeeded
                     -- (and @strs@ is the list of subexpression matches),
                     -- or 'Nothing' otherwise.
matchRegex p str = fmap (\(_,_,_,str) -> str) (matchRegexAll p str)

-- | Match a regular expression against a string, returning more information
-- about the match.
matchRegexAll
   :: Regex  -- ^ The regular expression
   -> String -- ^ The string to match against
   -> Maybe ( String, String, String, [String] )
                -- ^ Returns: 'Nothing' if the match failed, or:
                --
                -- >  Just ( everything before match,
                -- >         portion matched,
                -- >         everything after the match,
                -- >         subexpression matches )

matchRegexAll p str = matchM p str
