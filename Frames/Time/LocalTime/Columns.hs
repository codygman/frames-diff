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
module Frames.Time.LocalTime.Columns (MyColumns, TimeIn(..), Chicago(..)) where

import Frames (CommonColumns)
import Frames.ColumnTypeable (Parseable(..), Parsed(..))
import Frames.Time.LocalTime.TimeIn (timeIn, TimeIn(..), parseLocalTime)
import Data.Time.Zones (localTimeToUTCTZ)
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Monoid ((<>))
-- import Data.Time.Zones.TH

import Data.Thyme.Time
import Data.Time.Zones.Read (parseOlson)
import qualified Data.ByteString.Lazy.Char8 as C8



-- | Define a 'Parseable' instance for @TimeIn "America/Chicago"@
timeIn "America/Chicago"

-- instance Parseable (TimeIn "America/Chicago")
--     -- where parse = fmap (Definitely . (TimeIn . (toThyme . localTimeToUTCTZ (parseOlson (C8.pack "__TZDATA__"))))) . parseLocalTime
--     where parse txt = fmap (\localTime -> Definitely (TimeIn (toThyme (localTimeToUTCTZ ( {- timezone -} parseOlson (C8.pack "__TZDATA__")) localTime)))) (fromThyme <$> parseLocalTime txt)


-- | We need this newtype because Template Haskell can not handle the
-- type @TimeIn "America/Chicago"@ as of @GHC-8.0.1@ and
-- @template-haskell-2.11.0.0@
newtype Chicago = Chicago (TimeIn "America/Chicago") deriving Show

instance Parseable Chicago where
  parse = fmap (fmap Chicago) . parse

-- | The column types we expect our data to conform to
type MyColumns = Chicago ': CommonColumns


printTimeInTH = do
  txt <- T.pack . pprint <$> (runQ (timeIn "America/Chicago"))
  let txt' = T.unpack .
             T.replace "GHC.Base." "" .
             T.replace "Frames.ColumnTypeable." "".
             T.replace "Frames.Time.LocalTime.TimeIn." "" .
             T.replace "Data.Thyme.Time.Core." "" .
             T.replace "Data.Time.Zones.Read." "" .
             T.replace "Data.ByteString.Lazy.Char8." "" .
             (\(x,xs) -> T.replace "TZif2" "__TZDATA__" x <> T.takeWhileEnd (/= '"') xs) . T.breakOn "\\NUL"
             $ txt
  putStrLn txt'

