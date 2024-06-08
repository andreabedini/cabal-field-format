module Distribution.Pretty
  ( showToken,
    showTokenStr,
  )
where

import qualified Data.Char as Char
import Data.List (isPrefixOf, isSuffixOf)
import qualified Text.PrettyPrint as PP

showToken :: String -> PP.Doc
showToken = PP.text . showTokenStr

showTokenStr :: String -> String
showTokenStr str
  -- if token looks like a comment (starts with --), print it in quotes
  | "--" `isPrefixOf` str = show str
  -- also if token ends with a colon (e.g. executable name), print it in quotes
  | ":" `isSuffixOf` str = show str
  | not (any dodgy str) && not (null str) = str
  | otherwise = show str
  where
    dodgy c = Char.isSpace c || c == ','
