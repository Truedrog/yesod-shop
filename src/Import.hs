module Import
  ( module Import,
  )
where

import AppType as Import
import Foundation as Import
import Import.NoFoundation as Import
import Data.Aeson.Types (Pair)

stripNulls :: [Pair] -> [Pair]
stripNulls = filter (\(_, v) -> v /= Null)
