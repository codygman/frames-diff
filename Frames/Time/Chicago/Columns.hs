{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
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
module Frames.Time.Chicago.Columns (
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
import Frames hiding ((:&))
import Data.Hashable
import Frames.CSV (colQ,columnUniverse,rowGen)
import Frames.ColumnTypeable (Parseable(..))
import Frames.Time.Chicago.TimeIn
import Data.Time
import Data.Time.Lens
import Data.Time.Clock
import Data.Time.Zones
import Data.Time.Zones.TH
import GHC.Generics
import Frames.Default
import qualified Data.Vector as VB
import Frames.InCore (RecVec, VectorFor(..))
import Data.String (IsString(..))
import Control.Monad.Primitive (PrimMonad)
import Pipes (Pipe,Producer, (>->), runEffect)
import qualified Pipes.Prelude as P
import qualified Pipes as P

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

-- TODO reconsider whether using the ord instance of the raw LocalTime with no timezone is okay or not
instance Ord Chicago where
    (Chicago (TimeIn (ZonedTime lt1 _))) `compare` (Chicago (TimeIn (ZonedTime lt2 _))) = lt1 `compare` lt2
instance (IsString ZonedTime) where fromString = isStringZonedTime
instance Default (s :-> Chicago) where def = Col (Chicago (TimeIn "America/Chicago"))


type instance VectorFor Chicago = VB.Vector


-- | extracts the zoned time out of a Chicago type
chicagoToZoned = (\(Chicago (TimeIn zt)) -> zt)


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

-- TODO move these functions somewhere else that makes more sense
-- | Filters out records whose date isn't within the past N days
withinPastNDays
  :: (forall f. Functor f => ((Chicago -> f Chicago) -> Record rs -> f (Record rs)))
  -> Int
  -> Pipe (Record rs) (Record rs) IO r
withinPastNDays targetLens n = P.filterM (\r -> do
                                       now <- getCurrentTime
                                       pure $ (getDateFromRec r) >= (modL day (subtract n) now)
                                   )
  where getDateFromRec = zonedTimeToUTC . chicagoToZoned . rget targetLens


-- | Returns records whose target date falls between beginning of start Day and before start of end Day
dateBetween :: (PrimMonad m1) =>
               (forall f. Functor f => ((Chicago -> f Chicago) -> Record rs -> f (Record rs)))
            -> Day
            -> Day
            -> Pipe (Record rs) (Record rs) m1 m
dateBetween target start end = P.filter (\r -> let targetDate = (rget target r) :: Chicago
                                                   targetDate' = chicagoToZoned targetDate :: ZonedTime
                                                   targetDay = localDay (zonedTimeToLocalTime targetDate') :: Day
                                               in
                                                 targetDay >= start && targetDay < end
                                        )
{-# INLINABLE dateBetween #-}

-- | dateBetween for a Frame such as the result of `inCoreAoS (producer)`
-- example:
-- λ> F.length <$> dateBetween' transactionDate (inCoreAoS transactions) (d 2014 4 1) (d 2014 4 5)
-- 40
dateBetween' :: (RecVec fr, PrimMonad m) =>
                (forall f. Functor f => ((Chicago -> f Chicago) -> Record fr -> f (Record fr))) -- target lens
             -> m (FrameRec fr) -- in frame
             -> Day -- start
             -> Day -- end
             -> _ -- TODO can remove the monad wrapped FrameRec with runST I think, like: https://github.com/acowley/Frames/blob/122636432ab425f4cbf12fd400996eab78ef1462/src/Frames/InCore.hs#L215
dateBetween' target frame start end = do
  frame' <- frame
  -- ((dateBetween target start end) `onFrame` frame')
  ((dateBetween target start end) `onFrame` frame')
{-# INLINABLE dateBetween' #-}

onFrame :: (RecVec rs, PrimMonad m) =>  Pipe (Record rs) (Record rs) m () -> FrameRec rs -> m (FrameRec rs)
onFrame pipe f = inCoreAoS $ P.each f P.>-> pipe
{-# INLINABLE onFrame #-}
