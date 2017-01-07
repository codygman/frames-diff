{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Frames.Time.CTime.UnixTS ( MyColumns
                                 , Columns
                                 , Readable(..)
                                 , Parseable(..)
                                 , module Data.UnixTime
                                 , UnixTS(..)
                                 , unixToUTC
                                 , utcToUnix
                                 , utcToUnixTS
                                 , parseUTCTime
                                 , daysAgo
                                 , daysAhead
                                 , addUnixDiffTime
                                 , dt
                                 , daysAgo
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
import Data.Maybe
import Data.Time.LocalTime
import Data.Hashable
import GHC.Generics

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

dt :: Integer -> Int -> Int -> UnixTS
dt y m d = utcToUnixTS (UTCTime day' (0 :: DiffTime))
  where day' = Data.Time.fromGregorian y m d


getUnixTimeSOD = utcToUnix . zeroZonedTOD <$> Data.Time.getZonedTime
-- exmaple use/proof it works
-- λ> getZonedTime
-- 2017-01-06 19:15:05.120655981 CST
-- λ> tz <- getCurrentTimeZone
-- λ> utcToZonedTime tz . unixToUTC <$> getUnixTimeSOD
-- 2017-01-06 00:00:00 CST
-- λ> getZonedTime
-- 2017-01-06 19:15:21.050065071 CST

zeroZonedTOD :: Data.Time.ZonedTime -> Data.Time.UTCTime
zeroZonedTOD (Data.Time.ZonedTime (Data.Time.LocalTime ld _) tz) = Data.Time.zonedTimeToUTC $ Data.Time.ZonedTime (Data.Time.LocalTime ld (TimeOfDay 0 0 0)) tz

deriving instance Hashable UnixTS
deriving instance Generic UnixTS
deriving instance Generic UnixTime
deriving instance Hashable UnixTime
deriving instance Generic CTime
deriving instance Hashable CTime

daysAgo' n = secondsToUnixDiffTime (- n*60*60*24)
daysAhead' n = secondsToUnixDiffTime (n*60*60*24)

daysAgo :: UnixTime -> Integer -> UnixTS
daysAgo now n = UnixTS $ now `addUnixDiffTime` daysAgo' n

daysAhead :: UnixTime -> Integer -> UnixTS
daysAhead now n = UnixTS $ now `addUnixDiffTime` daysAhead' n


daysAgoUTCTm :: UTCTime -> Integer -> UnixTS
daysAgoUTCTm now n = UnixTS $ (utcToUnix now :: UnixTime) `addUnixDiffTime` daysAgo' n

 -- we'll only consider trying to parse date strings from text from:
      -- 1997-07-16                        -- min: 10 characters
      -- 1997-07-16T19:20:30.45+01:00      -- max: 28 characters
      -- NO! I changed max to 100 characters because I got bit by this ;) considering taking it out!
      -- this instance will affect repl reload time and compile time
      -- so best to short circuit text that isn't within these ranges
      -- TODO use parseUnixTime here... but it needs to be more restrictive than accepting "000000","", or "0" as a valid timestamp
      -- | T.length t >= 10 && T.length t <= 28 = msum $  (($ C8pack t) . parseUnixTime) <$> formats
      -- | T.length t >= 10 && T.length t <= 100 = parseUTCTime' t
      -- our time text string is also required to have one or more dashes
parseUTCTime txt
  | T.length txt >= 10 && T.length txt <= 100 && (isJust (T.find  (== '-') txt)) = msum (map (\ fmt -> pure . UnixTS . parseUnixTime fmt $ TE.encodeUtf8 txt) formats)
  | otherwise = mzero

instance Readable UnixTS  where
  fromText t = parseUTCTime t

formats :: [C8.ByteString]
formats = [ "%F %T"
          , "%F"
          , "%F %T"
          , "%F %T %z %Z"
            -- some european formats
          , "%d/%m/%Y"
          ]


instance Parseable UnixTS

type MyColumns = UnixTS ': CommonColumns

instance Default (s :-> UnixTS) where def = Col (UnixTS $ fromEpochTime (0 :: EpochTime))
