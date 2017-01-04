{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Define the 'TimeIn' type that lets us specify in the type how a
-- 'LocalTime' should be converted to a 'UTCTime'.
module Frames.Time.LocalTime.TimeIn where
import Control.Monad (MonadPlus, msum, mzero)
import qualified Data.Text as T
import Data.Thyme.Time
import Data.Thyme.Clock
import System.Locale (defaultTimeLocale)
-- import Data.Time.Zones
import Data.Time.Zones.TH (includeTZFromDB)
import Frames.ColumnTypeable (Parseable(..), Parsed(..))
import GHC.TypeLits
import Language.Haskell.TH
import Data.Maybe (fromJust)

-- | A 'UTCTime' tagged with a symbol denoting the 'TZ' time zone from
-- whence it came.
newtype TimeIn (zone :: Symbol) = TimeIn Data.Thyme.Clock.UTCView deriving Show

-- | Try to parse a 'LocalTime' value using common formats.
parseLocalTime :: MonadPlus m => T.Text -> m Data.Thyme.Time.LocalTime
parseLocalTime timeStr = msum funcs
  where formats =  [ "%F %T"
                   , "%F"
                   , "%F %T"
                   , "%F %T %z %Z"

                     -- some european formats
                   , "%d/%m/%Y"
                   ]
        funcs = map (\fmt -> do
                        case mkParser fmt (T.unpack timeStr) of
                          Just f -> pure f
                          Nothing -> mzero
                    ) formats
        mkParser = parseTime defaultTimeLocale

{-# INLINABLE parseLocalTime #-}

-- | @zonedTime "America/Chicago"@ will create a 'Parseable' instance
-- for the type @TimeIn "America/Chicago"@. You can then use this type
-- when loading data.
timeIn :: String -> DecsQ
timeIn tzStr =
  do let fromLocal = [e| localTimeToUTCTZ $(includeTZFromDB tzStr) |]
         ex = [e| fmap (Definitely . TimeIn . toThyme . $fromLocal) . (fmap fromThyme . parseLocalTime) |]
     sequenceA [
       instanceD (pure [])
                 [t|Parseable (TimeIn $(pure $ LitT (StrTyLit tzStr)))|]
                 [ funD (mkName "parse") [clause [] (normalB ex) []] ] ]


-- -- | Try to parse a 'LocalTime' value using common formats.
-- parseLocalTime :: MonadPlus m => T.Text -> m LocalTime
-- parseLocalTime t = msum (map (($ T.unpack t) . mkParser) formats)
--   where formats = [ "%F %T"
--                   , "%F"
--                   , "%F %T"
--                   , "%F %T %z %Z"

--                   -- some european formats
--                   , "%d/%m/%Y"
--                   ]
--         mkParser = parseTimeM True defaultTimeLocale -- TODO should I use a different time locale?

