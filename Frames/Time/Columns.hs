{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Define the column types used to represent our data. Here, we wish
-- to parse data captured as 'Data.Time.LocalTime.LocalTime' values
-- into the \"America/Chicago\" time zone.
module Frames.Time.Columns (
    MyColumns
  , TimeIn(..)
  , Chicago(..)
  , colQ
  , columnUniverse
  , rowGen
  -- , chicagoToZonedTime
  -- , zonedTimeToChicago
  , addSecondsChicago
  ) where
import Frames (CommonColumns)
import Data.Hashable
import Frames.CSV (colQ,columnUniverse,rowGen)
import Frames.ColumnTypeable (Parseable(..))
import Frames.Time.TimeIn
import Data.Time
import Data.Time.Clock
import Data.Time.Zones
import Data.Time.Zones.TH
import GHC.Generics

-- | Define a 'Parseable' instance for @TimeIn "America/Chicago"@
timeIn "America/Chicago"

-- | We need this newtype because Template Haskell can not handle the
-- type @TimeIn "America/Chicago"@ as of @GHC-8.0.1@ and
-- @template-haskell-2.11.0.0@
newtype Chicago = Chicago (TimeIn "America/Chicago") deriving (Show)

instance Parseable Chicago where
  parse = fmap (fmap Chicago) . parse

instance Eq Chicago where
   (Chicago (TimeIn zndTm)) == (Chicago (TimeIn zndTm')) = zndTm == zndTm'

-- instance Eq ZonedTime where
--   a == b = zonedTimeToUTC a == zonedTimeToUTC b

deriving instance Hashable Chicago

-- | The column types we expect our data to conform to
type MyColumns = Chicago ': CommonColumns

tzChicago :: TZ
tzChicago = $(includeTZFromDB "America/Chicago")


-- chicagoToZonedTime :: Chicago -> ZonedTime
-- chicagoToZonedTime (Chicago timeIn) = go timeIn
--   where go (TimeIn utcTm) = do
--           let tz = timeZoneForUTCTime tzChicago utcTm
--           utcToZonedTime tz utcTm

-- zonedTimeToChicago :: ZonedTime -> Chicago
-- zonedTimeToChicago zt = do
--   let utcTm = zonedTimeToUTC zt
--   Chicago (TimeIn utcTm)

addSecondsChicago :: NominalDiffTime -> Chicago -> Chicago
addSecondsChicago seconds (Chicago timeIn) = go timeIn
  where go (TimeIn zndTm) = Chicago (TimeIn zndTm')
          where utcTm = zonedTimeToUTC (zndTm :: ZonedTime) :: UTCTime
                tmZn = zonedTimeZone zndTm
                zndTm' = utcToZonedTime tmZn (addUTCTime seconds utcTm) :: ZonedTime

-- λ> :t \r -> set birthday (chicagoToZonedTime (view birthday r))
