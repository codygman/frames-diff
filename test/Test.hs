{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
-- import Test.QuickCheck.Instances 
import Control.Exception (evaluate)
-- import Data.Time.Clock.POSIX
-- import qualified Data.Time --  (UTCTime,)
import System.Posix.Types
-- import Data.Time.Calendar
import Data.Maybe (isJust,isNothing)
import Data.Thyme
import Frames.Time.LocalTime
import Frames.ColumnTypeable

-- timeParseUTCTimeFromSecondsThenShow = show . posixSecondsToUTCTime . realToFrac . utcTimeToPOSIXSeconds

-- instance Arbitrary Data.Time.Day where
--     arbitrary = Data.Time.ModifiedJulianDay <$> (unixEpochJulian +) <$> arbitrary
--       where unixEpochJulian = 40587
--     shrink    = (Data.Time.ModifiedJulianDay <$>) . shrink . Data.Time.toModifiedJulianDay

-- instance Arbitrary Data.Time.DiffTime where
--     arbitrary = arbitrarySizedFractional
--     shrink    = (fromRational <$>) . shrink . toRational

-- instance Arbitrary Data.Time.UTCTime where
--     arbitrary =
--         Data.Time.UTCTime
--         <$> arbitrary
--         <*> (fromRational . toRational <$> choose (0::Double,0))
--     shrink ut@(Data.Time.UTCTime day dayTime) =
--         [ ut { Data.Time.utctDay     = d' } | d' <- shrink day     ] ++
--         [ ut { Data.Time.utctDayTime = t' } | t' <- shrink dayTime ]


main :: IO ()
main = hspec $ do
  describe "Frames.Time" $ do
    describe "PosixTime" $ do
      describe "fuzzyParseLocalTime" $ do
        describe "Should successfully parse these values" $ do
          it "should give a default value for bad dates" $
            (fuzzyParseLocalTime "" :: Maybe LocalTime) `shouldBe` pure zeroLocalTm
          it "should parse %F" $
            (fuzzyParseLocalTime "2016-05-03" :: Maybe LocalTime) `shouldSatisfy` isJust
          it "should parse %F %T" $
            (fuzzyParseLocalTime "2016-05-03 00:00:00" :: Maybe LocalTime) `shouldSatisfy` isJust
          it "should parse %F %T %z %Z" $
            (fuzzyParseLocalTime "2016-05-03 00:00:00 +0006 CST" :: Maybe LocalTime) `shouldSatisfy` isJust
          it "should parse '%d/%m/%Y'" $
            (fuzzyParseLocalTime "03/05/2016" :: Maybe LocalTime) `shouldSatisfy` isJust

        describe "LocalTime Parseable instance" $ do
          describe "Should successfully parse these values" $ do
            it "should give a default value for bad dates" $
              (parse "" :: Maybe (Parsed LocalTime)) `shouldBe` (Just . Definitely $ zeroLocalTm)
            it "should parse %F" $
              (parse "%F" :: Maybe (Parsed LocalTime)) `shouldSatisfy` isJust
            it "should parse %F %T" $
              (parse "2016-05-03 00:00:00" :: Maybe (Parsed LocalTime)) `shouldSatisfy` isJust
            it "should parse %F %T %z %Z" $
              (parse "2016-05-03 00:00:00 +0006 CST" :: Maybe (Parsed LocalTime)) `shouldSatisfy` isJust
            it "should parse '%d/%m/%Y'" $
              (parse "03/05/2016" :: Maybe (Parsed LocalTime)) `shouldSatisfy` isJust

            --   it "unixToUTC and utcToUnix are isomorphic" $ property $
  --     \utctm -> (unixToUTC . utcToUnix $ (utctm :: Data.Time.UTCTime) :: Data.Time.UTCTime) `shouldBe` (utctm :: Data.Time.UTCTime)
  --   it "unixTS show instance is exactly the same as UTCTime show instance" $ property $
  --     \x -> show (utcToUnixTS (x :: Data.Time.UTCTime) :: UnixTS) ==
  --              timeParseUTCTimeFromSecondsThenShow (x :: Data.Time.UTCTime)
  --   it "parses valid time text 1" $
  --     parseUTCTime "2017-01-06 00:00:00 +0000 UTC" `shouldSatisfy` isJust
  --   it "parses valid time text 2" $
  --     parseUTCTime "2017-01-06 00:00:00" `shouldSatisfy` isJust
  --   it "parses valid time text 3" $
  --     parseUTCTime "2017-01-06" `shouldSatisfy` isJust
  --   it "does not parse a date with no spaces" $
  --     parseUTCTime "20170106" `shouldSatisfy` isNothing
  -- it "does not parse an empty string" $
  --     parseUTCTime "" `shouldSatisfy` isNothing



-- TODO
-- tableTypes' rowGen { rowTypeName = "A"} "data/a.txt"
-- tableTypes' rowGen { rowTypeName = "B"} "data/b.txt"

-- as :: Producer A IO ()
-- as = readTable "data/a.txt"

-- bs :: Producer B IO ()
-- bs = readTable "data/b.txt"

-- singA =  head . F.toList <$> inCoreAoS as

-- singB = head . F.toList <$> inCoreAoS bs
