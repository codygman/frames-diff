{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
-- import Test.QuickCheck.Instances 
import Control.Exception (evaluate)
import Data.Time.Clock.POSIX
import qualified Data.Time --  (UTCTime,)
import System.Posix.Types
import Data.UnixTime
import Frames.Time.CTime.UnixTS
import Data.Time.Calendar
import Data.Maybe (isJust)

timeParseUTCTimeFromSecondsThenShow = show . posixSecondsToUTCTime . realToFrac . utcTimeToPOSIXSeconds

instance Arbitrary Data.Time.Day where
    arbitrary = Data.Time.ModifiedJulianDay <$> (unixEpochJulian +) <$> arbitrary
      where unixEpochJulian = 40587
    shrink    = (Data.Time.ModifiedJulianDay <$>) . shrink . Data.Time.toModifiedJulianDay

instance Arbitrary Data.Time.DiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = (fromRational <$>) . shrink . toRational

instance Arbitrary Data.Time.UTCTime where
    arbitrary =
        Data.Time.UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0::Double,0))
    shrink ut@(Data.Time.UTCTime day dayTime) =
        [ ut { Data.Time.utctDay     = d' } | d' <- shrink day     ] ++
        [ ut { Data.Time.utctDayTime = t' } | t' <- shrink dayTime ]


main :: IO ()
main = hspec $ do
  describe "UnixTS" $ do
    it "unixToUTC and utcToUnix are isomorphic" $ property $
      \utctm -> (unixToUTC . utcToUnix $ (utctm :: Data.Time.UTCTime) :: Data.Time.UTCTime) `shouldBe` (utctm :: Data.Time.UTCTime)
    it "unixTS show instance is exactly the same as UTCTime show instance" $ property $
      \x -> show (utcToUnixTS (x :: Data.Time.UTCTime) :: UnixTS) ==
               timeParseUTCTimeFromSecondsThenShow (x :: Data.Time.UTCTime)
    it "parses valid time text 1" $
      parseUTCTime' "2017-01-06 00:00:00 +0000 UTC" `shouldSatisfy` isJust
    it "parses valid time text 2" $
      parseUTCTime' "2017-01-06 00:00:00" `shouldSatisfy` isJust
    it "parses valid time text 3" $
      parseUTCTime' "2017-01-06" `shouldSatisfy` isJust



-- TODO
-- tableTypes' rowGen { rowTypeName = "A"} "data/a.txt"
-- tableTypes' rowGen { rowTypeName = "B"} "data/b.txt"

-- as :: Producer A IO ()
-- as = readTable "data/a.txt"

-- bs :: Producer B IO ()
-- bs = readTable "data/b.txt"

-- singA =  head . F.toList <$> inCoreAoS as

-- singB = head . F.toList <$> inCoreAoS bs
