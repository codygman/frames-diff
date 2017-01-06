{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Frames.Time.CTime.UnixTS ( MyColumns
                                 , Columns
                                 , Readable(..)
                                 , Parseable(..)
                                 , module Data.UnixTime
                                 , UnixTS(..)
                                 , unixToUTC
                                 , utcToUnix
                                 , utcToUnixTS
  ) where

import Foreign.C.Types
import Frames
import Frames.InCore (RecVec, VectorFor(..))
import Frames.ColumnTypeable
import Data.Readable (Readable(..))
import qualified Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Posix.Types
import Data.UnixTime
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as UB
import qualified Data.Vector as VB
import Control.Monad
import Data.UnixTime (UnixTime(..), parseUnixTime, toEpochTime, getUnixTime)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C8
import Frames.Default

newtype UnixTS = UnixTS UnixTime
-- TODO update to use unboxed vector if possible
type instance VectorFor UnixTS = VB.Vector

instance Show UnixTS where
  show (UnixTS utm@(UnixTime seconds micros)) = show . posixSecondsToUTCTime . realToFrac . toEpochTime $ utm -- show (realToFrac seconds :: PosixTime)

instance Ord UnixTS where
  (UnixTS ut1) `compare` (UnixTS ut2) = ut1 `compare` ut2

instance Eq UnixTS where
  (UnixTS ut1) == (UnixTS ut2) = ut1 == ut2

epoch :: Data.Time.UTCTime
epoch = Data.Time.UTCTime (Data.Time.fromGregorian 1970 1 1) 0

unixToUTC :: UnixTime -> Data.Time.UTCTime
unixToUTC = posixSecondsToUTCTime . realToFrac . toEpochTime

epochTime :: Data.Time.UTCTime -> EpochTime
epochTime = fromIntegral . floor . flip Data.Time.diffUTCTime epoch

utcToUnix :: Data.Time.UTCTime -> UnixTime
utcToUnix = fromEpochTime . epochTime

utcToUnixTS :: Data.Time.UTCTime -> UnixTS
utcToUnixTS = UnixTS . utcToUnix



todayEpoch = toEpochTime <$> getUnixTime

instance Readable UnixTS  where
  fromText t
      -- we'll only consider trying to parse date strings from text from:
      -- 1997-07-16                        -- min: 10 characters
      -- 1997-07-16T19:20:30.45+01:00      -- max: 28 characters
      -- this instance will affect repl reload time and compile time
      -- so best to short circuit text that isn't within these ranges
      -- TODO use parseUnixTime here... but it needs to be more restrictive than accepting "000000","", or "0" as a valid timestamp
      -- | T.length t >= 10 && T.length t <= 28 = msum $  (($ C8pack t) . parseUnixTime) <$> formats
      | T.length t >= 10 && T.length t <= 28 = msum (map (\ d -> mkParser' d (T.unpack t)) formats)
      | otherwise = mzero
    where formats :: [String]
          formats = [ "%F %T"
                    , "%F"
                    , "%F %T"
                    , "%F %T %z %Z"

                    -- some european formats
                    , "%d/%m/%Y"
                    ]
          mkParser fmt txt = Data.Time.parseTimeM True Data.Time.defaultTimeLocale fmt txt -- TODO should I use a different time locale?
          mkParser' fmt txt  = utcToUnixTS <$> mkParser fmt txt

instance Parseable UnixTS where
  parse = fmap Possibly . fromText

type MyColumns = UnixTS ': CommonColumns

instance Default (s :-> UnixTS) where def = Col (UnixTS $ fromEpochTime (0 :: EpochTime))
