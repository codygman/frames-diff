{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Define the 'TimeIn' type that lets us specify in the type how a
-- 'LocalTime' should be converted to a 'UTCTime'.
module Frames.Time.TimeIn where
import Control.Monad (MonadPlus, msum)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.TH
import Frames.ColumnTypeable (Parseable(..), Parsed(..))
import GHC.TypeLits
import Language.Haskell.TH
import Data.Maybe (isJust)

-- | A 'UTCTime' tagged with a symbol denoting the 'TZ' time zone from
-- whence it came.
newtype TimeIn (zone :: Symbol) = TimeIn ZonedTime deriving Show

-- | Try to parse a 'LocalTime' value using common formats.
parseLocalTime :: MonadPlus m => T.Text -> m LocalTime
parseLocalTime t = msum (map (($ T.unpack t) . mkParser) formats)
  where formats = ["%F %T", "%F", "%F %T", "%F %T %z %Z"]
        mkParser = parseTimeM True defaultTimeLocale -- TODO should I use a different time locale?

isStringZonedTime :: String -> ZonedTime
isStringZonedTime str = case (filter isJust (map ($ str) (map mkParser formats) :: [Maybe ZonedTime])) of
                        (x:_) -> case (x :: Maybe ZonedTime) of -- takes the first format matched
                          Just tm -> tm
                          Nothing -> do
                            -- in case of an error provide an obviously wrong date value
                            ZonedTime (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0)) tzZero
                        _ -> do
                          -- in case of an error provide an obviously wrong date value
                            ZonedTime (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0)) tzZero
  where utcTmZero = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
        tzZero = timeZoneForUTCTime $(includeTZFromDB "America/Chicago") utcTmZero


formats = ["%F %T", "%F", "%F %T", "%F %T %z %Z"]
mkParser = parseTimeM True defaultTimeLocale


-- | @zonedTime "America/Chicago"@ will create a 'Parseable' instance
-- for the type @TimeIn "America/Chicago"@. You can then use this type
-- when loading data.
-- TODO this is returning a UTCTime still.... needs to return ZonedTime
-- TODO what should the splice look like for a ZonedTime, it was previously UTCTime
{-

Here are their types:

UTCTime :: Day -> DiffTime -> UTCTime
ZonedTime :: LocalTime -> TimeZone -> ZonedTime

So the Parseable instance below:

[t| Parseable (TimeIn $(pure $ LitT (StrTyLit tzStr)))|]

Needs to be updated to... wait.. is it as simple as putting the tz after timeIn in the ex declaration?

That didn't seem to do it... so on to how that changed the TimeIn type:

Previously with UTCTime:

TimeIn :: UTCTime -> TimeIn zone
UTCTime :: Day -> DiffTime -> UTCTime
DiffTime just used the numeric instance before I think

Now with ZonedTime:

TimeIn :: ZonedTime -> TimeIn zone
ZonedTime :: LocalTime -> TimeZone -> ZonedTime

So the TimeIn for UTCTime was (is? hopefully not for too long):

(TimeIn $(pure $ LitT (StrTyLit tzStr)))

So that translates to:

TimeIn "America/Chicago"

Back to the original problem, our type error is:

Frames/Time/Columns.hs:33:1: warning: [-Wdeferred-type-errors] …
    • Couldn't match expected type ‘ZonedTime’ with actual type ‘TZ’
    • In the first argument of ‘TimeIn’, namely
        ‘Data.Time.Zones.Read.parseOlson

The squiggly is under timeIn, so I think it's generating a TimeIn (TZ) when a TimeIn (ZonedTime) is being looked for.

Hmm... what was the type of ZonedTime again?

ZonedTime :: LocalTime -> TimeZone -> ZonedTime

So the local time needs to be passed directly first????? Maybe like:

let fromLocal = [e| localTimeToUTCTZ $(includeTZFromDB tzStr) |]

That seems to be closer, new type error is:

Frames/Time/Columns.hs:33:1: warning: [-Wdeferred-type-errors] …
    • Couldn't match type ‘UTCTime’ with ‘ZonedTime’
      Expected type: LocalTime -> ZonedTime
        Actual type: LocalTime -> UTCTime
    • In the second argument of ‘(.)’, namely
        ‘(localTimeToUTCTZ
            (Data.Time.Zones.Read.parseOlson

Wait what was the type of localTimeToUTCTZ?
λ> :t localTimeToUTCTZ
localTimeToUTCTZ :: TZ -> LocalTime -> UTCTime


-}
timeIn :: String -> DecsQ
timeIn tzStr =
  do
    let fromLocal = [e| (\utcTm' -> utcToZonedTime ((timeZoneForUTCTime $(includeTZFromDB tzStr) utcTm') :: TimeZone) utcTm') . localTimeToUTCTZ $(includeTZFromDB tzStr) |]
        ex = [e| fmap (Definitely . TimeIn . $fromLocal) . parseLocalTime |]
    sequenceA [
      instanceD (pure [])
        [t| Parseable (TimeIn $(pure $ LitT (StrTyLit tzStr)))|]
        [ funD (mkName "parse") [clause [] (normalB ex) []] ] ]

dateInPast :: Integer -> ZonedTime -> ZonedTime -> Bool
dateInPast n inDt cmpDt  = (localDay . zonedTimeToLocalTime $ inDt) >= addDays (- n) (localDay . zonedTimeToLocalTime $ cmpDt)
