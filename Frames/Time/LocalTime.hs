{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Frames.Time.LocalTime where

import qualified Data.Text as T
import Data.Thyme
import Data.Thyme.Time.Core (fromGregorian, secondsToDiffTime)
import Control.Monad (MonadPlus,msum,mzero)
import Data.Text hiding (map)
import Data.Maybe (fromMaybe)
import System.Locale (defaultTimeLocale)
import Frames.Default
import Frames.ColumnTypeable
import Frames
import Data.Readable
import qualified Data.Vector as VB
import Frames.InCore


-- parseLocalTime :: String -> String -> Maybe LocalTime
parseLocalTime fmt str = case parseTime defaultTimeLocale fmt str of
                           Just t -> pure t
                           Nothing -> mzero

-- parseLocalTimeDef fmt str = fromMaybe zeroLocalTm (parseLocalTime fmt str)
zeroLocalTm = LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 (secondsToDiffTime 0))

fuzzyParseLocalTime :: MonadPlus m => Text -> m LocalTime
fuzzyParseLocalTime txt = msum (map (\ d -> parseLocalTime d (T.unpack txt)) formats)

formats :: [String]
formats = [ "%F %T"
          , "%F"
          , "%F %T"
          , "%F %T %z %Z"
            -- some european formats
          , "%d/%m/%Y"
          ]

-- frames instances
instance Parseable LocalTime

instance Readable LocalTime  where
  fromText = fuzzyParseLocalTime

type MyColumns = LocalTime ': CommonColumns
instance Default (s :-> LocalTime) where def = Col zeroLocalTm
type instance VectorFor LocalTime = VB.Vector
