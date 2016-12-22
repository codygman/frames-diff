{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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
  , chicagoToZonedTime
  , zonedTimeToChicago
  ) where
import Frames (CommonColumns)
import Frames.CSV (colQ,columnUniverse,rowGen)
import Frames.ColumnTypeable (Parseable(..))
import Frames.Time.TimeIn
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.TH

-- | Define a 'Parseable' instance for @TimeIn "America/Chicago"@
timeIn "America/Chicago"

-- | We need this newtype because Template Haskell can not handle the
-- type @TimeIn "America/Chicago"@ as of @GHC-8.0.1@ and
-- @template-haskell-2.11.0.0@
newtype Chicago = Chicago (TimeIn "America/Chicago") deriving Show

instance Parseable Chicago where
  parse = fmap (fmap Chicago) . parse

-- | The column types we expect our data to conform to
type MyColumns = Chicago ': CommonColumns

tzChicago :: TZ
tzChicago = $(includeTZFromDB "America/Chicago")


chicagoToZonedTime :: Chicago -> ZonedTime
chicagoToZonedTime (Chicago timeIn) = go timeIn
  where go (TimeIn utcTm) = do
          let tz = timeZoneForUTCTime tzChicago utcTm
          utcToZonedTime tz utcTm

zonedTimeToChicago :: ZonedTime -> Chicago
zonedTimeToChicago zt = do
  let utcTm = zonedTimeToUTC zt
  Chicago (TimeIn utcTm)
