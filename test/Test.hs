{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Maybe (isJust,isNothing)
import Frames.Diff
import Frames
import Data.Text
import Data.Proxy
import qualified Pipes as P
import Control.Monad.IO.Class
import qualified Data.Foldable as F

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

type Customer = Record '[CustomerId, "CustomerName" :-> Text, "ContactName" :-> Text, "Address" :-> Text, "City" :-> Text, "PostalCode" :-> Int, "Country" :-> Text]
type Order = Record '["OrderId" :-> Int, CustomerId, "EmployeeID" :-> Int, "OrderDate" :-> Text, "ShipperID" :-> Int]
type CustomerId = "customerID" :-> Int
customerId ::
  forall f rs. (Functor f, CustomerId ∈ rs) =>
  (Int -> f Int) -> Record rs -> f (Record rs)
customerId = rlens (Proxy :: Proxy CustomerId)

type UserId = "userId" :-> Int
type Simple1 = Record '["auto" :-> Int, UserId]
type Simple2 = Record '[UserId, "uslessCol" :-> Text]
userId ::
  forall f rs. (Functor f, UserId ∈ rs) =>
  (Int -> f Int) -> Record rs -> f (Record rs)
userId = rlens (Proxy :: Proxy UserId)


main :: IO ()
main = do
  let simple1 :: [Simple1]
      simple1 = [ 0 &: 100 &: Nil
                , 1 &: 100 &: Nil
                , 2 &: 100 &: Nil
                , 3 &: 100 &: Nil
                ]
      simple2 :: [Simple2]
      simple2 = [ 100 &: "usless" &: Nil]

  joinedSimple         <- F.toList <$> innerJoin (P.each simple1) userId (P.each simple2) userId
  joinedSimpleFlipped         <- F.toList <$> innerJoin (P.each simple2) userId (P.each simple1) userId


  hspec $ do
    describe "UnixTS" $ do
      it "unixToUTC and utcToUnix are isomorphic" $ property $
        \utctm -> (unixToUTC . utcToUnix $ (utctm :: Data.Time.UTCTime) :: Data.Time.UTCTime) `shouldBe` (utctm :: Data.Time.UTCTime)
      it "unixTS show instance is exactly the same as UTCTime show instance" $ property $
        \x -> show (utcToUnixTS (x :: Data.Time.UTCTime) :: UnixTS) ==
              timeParseUTCTimeFromSecondsThenShow (x :: Data.Time.UTCTime)
      it "parses valid time text 1" $
        parseUTCTime "2017-01-06 00:00:00 +0000 UTC" `shouldSatisfy` isJust
      it "parses valid time text 2" $
        parseUTCTime "2017-01-06 00:00:00" `shouldSatisfy` isJust
      it "parses valid time text 3" $
        parseUTCTime "2017-01-06" `shouldSatisfy` isJust
      it "does not parse a date with no spaces" $
        parseUTCTime "20170106" `shouldSatisfy` isNothing
      it "does not parse an empty string" $
        parseUTCTime "" `shouldSatisfy` isNothing

      describe "innerJoin" $ do
{-
-- an inner join on simple1/simple2 data in sqlite
cody@zentop:~/source/frames-diff/test$ sqlite3 test.sqlite
SQLite version 3.11.0 2016-02-15 17:29:24
Enter ".help" for usage hints.
sqlite> create table simple1(auto,uid);
sqlite> create table simple2(uid,uselessCol);
sqlite> insert into simple1(auto,uid) values(1,100);
sqlite> insert into simple1(auto,uid) values(2,100);
sqlite> insert into simple1(auto,uid) values(3,100);
sqlite> insert into simple2(uid,uselessCol) values(100,"useless");
sqlite> select * from simple1 inner join simple2 on simple1.uid = simple2.uid;
0|100|100|useless
1|100|100|useless
2|100|100|useless
3|100|100|useless
sqlite> select * from simple2 inner join simple1 on simple2.uid = simple1.uid;
100|useless|1|100
100|useless|2|100
100|useless|3|100
-}
        it "gives multiple results back when one table contains only one row and there are multiple matches" $ do
          joinedSimple `shouldBe`
            [ 0 &: 100 &: 100 &: "usless" &: Nil
            , 1 &: 100 &: 100 &: "usless" &: Nil
            , 2 &: 100 &: 100 &: "usless" &: Nil
            , 3 &: 100 &: 100 &: "usless" &: Nil
            ]
        -- TOOD this matches what sqlite does, but it could cause complications for callers who expect a specific "type" (or in vinyl's case collection of coolumns in a certain order)
        it "sadly it has a flipped type when you reorder arguments" $ do
          joinedSimpleFlipped `shouldBe`
            [ 100 &: "usless" &: 0 &: 100 &: Nil
            , 100 &: "usless" &: 1 &: 100 &: Nil
            , 100 &: "usless" &: 2 &: 100 &: Nil
            , 100 &: "usless" &: 3 &: 100 &: Nil
            ]


-- TODO
-- tableTypes' rowGen { rowTypeName = "A"} "data/a.txt"
-- tableTypes' rowGen { rowTypeName = "B"} "data/b.txt"

-- as :: Producer A IO ()
-- as = readTable "data/a.txt"

-- bs :: Producer B IO ()
-- bs = readTable "data/b.txt"

-- singA =  head . F.toList <$> inCoreAoS as

-- singB = head . F.toList <$> inCoreAoS bs

-- TODO complete these northwind tests/examples
-- northwind data sample
{-
=============================================== Customers ===============================================
CustomerID |  CustomerName                       |  ContactName    |  Address                       |  City        |  PostalCode |  Country
1          |  Alfreds Futterkiste                |  Maria Anders   |  Obere Str. 57                 |  Berlin      |  12209      |  Germany
2          |  Ana Trujillo Emparedados y helados |  Ana Trujillo   |  Avda. de la Constitución 2222 |  México D.F. |  05021      |  Mexico
3          |  Antonio Moreno Taquería            |  Antonio Moreno |  Mataderos 2312                |  México D.F. |  05023      |  Mexico
=========================================================================================================

=============================================== Orders ===============================================
OrderID |  CustomerID |  EmployeeID |  OrderDate  |  ShipperID
10308   |  2          |  7          |  1996-09-18 |  3
10309   |  37         |  3          |  1996-09-19 |  1
10310   |  77         |  8          |  1996-09-20 |  2
=========================================================================================================

-}


  -- let customers :: [Customer]
  --     customers = [ 1 &: "Alfreds Futterkiste"                &: "Maria Anders"   &: "Obere Strr. 57"                &: "Berlin"      &: 12209 &: "Germany" &: Nil
  --                 , 2 &: "Ana Trujillo Emparedados y helados" &: "Ana Trujillo"   &: "Avda. de la Constitución 2222" &: "México D.F." &: 05021 &: "Mexico"  &: Nil
  --                 , 3 &: "Antonio Moreno Taquería"            &: "Antonio Moreno" &: "Mataderos 2312"                &: "México D.F." &: 05023 &: "Mexico"  &: Nil
  --                 ]
  --     orders :: [Order]
  --     orders = [ 10308 &: 2 &: 7 &: "1996-09-18" &: 3 &: Nil
  --              , 10309 &: 3 &: 3 &: "1996-09-19" &: 1 &: Nil
  --              , 10310 &: 77 &: 8 &: "1996-09-20" &: 2 &: Nil
  --              ]
  --     orders' :: [Order]
  --     orders' = [ 10308 &: 2 &: 7 &: "1996-09-18" &: 3 &: Nil ]

-- it "gives correct result regardless of left/right tables with 3 customers rows and 3 orders" $ do
        --   joinedCustomersLeft1 `shouldBe`
        --     [ 2 &: "Ana Trujillo Emparedados y helados" &: "Ana Trujillo"   &: "Avda. de la Constitución 2222" &: "México D.F." &: 05021 &: "Mexico" &: 10308 &: 2 &: 7 &: "1996-09-18"  &: 3 &: Nil
        --     ]
        -- it "gives correct result regardless of left/right tables with 3 customers rows and 1 order" $ do
        --   joinedOrdersLeft1 `shouldBe`
        --     [ 10308 &: 2  &: 7 &: "1996-09-18" &: 3 &: 2 &: "Ana Trujillo Emparedados y helados" &: "Ana Trujillo"   &: "Avda. de la Constitución 2222" &: "México D.F." &: 05021 &: "Mexico" &: Nil
        --     ]

  -- joinedCustomersLeft3 <- F.toList <$> innerJoin (P.each customers) customerId (P.each orders) customerId
  -- joinedOrdersLeft3    <- F.toList <$> innerJoin (P.each orders) customerId (P.each customers) customerId

  -- joinedCustomersLeft1 <- F.toList <$> innerJoin (P.each customers) customerId (P.each orders') customerId
  -- joinedOrdersLeft1    <- F.toList <$> innerJoin (P.each orders') customerId (P.each customers) customerId

