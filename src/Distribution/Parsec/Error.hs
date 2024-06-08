{-# LANGUAGE DeriveGeneric #-}

module Distribution.Parsec.Error
  ( PError (..),
    showPError,
  )
where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Distribution.Parsec.Position
import GHC.Generics (Generic)
import System.FilePath (normalise)

-- | Parser error.
data PError = PError Position String
  deriving (Show, Generic)

instance Binary PError

instance NFData PError

showPError :: FilePath -> PError -> String
showPError fpath (PError pos msg) =
  normalise fpath ++ ":" ++ showPos pos ++ ": " ++ msg
