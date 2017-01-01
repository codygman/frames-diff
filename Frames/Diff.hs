{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frames.Diff ( defaultingProducer
                   , findMissingRowsOn
                   , Default(..)
                   , dt
                   , withinPastNDays
                   , dateBetween
                   , distinctOn
                   , innerJoin
                   ) where

import Frames hiding ((:&))
import Frames.CSV (RowGen(..), ReadRec)
import Control.Monad.IO.Class (MonadIO)
import Data.Vinyl (Rec, RecApplicative(rpure), rmap, rapply)
import Data.Vinyl.Functor (Lift(..), Identity(..))
import Data.Vinyl.TypeLevel
import Control.Lens (view, (&), (?~))
import Pipes (Pipe,Producer, (>->), runEffect)
import Data.Monoid ((<>),First(..))
import Data.Maybe (fromMaybe)
import Control.Lens.Getter(Getting)
import qualified Pipes.Prelude as P
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M
import qualified Data.Text.Format as T
import Data.Int (Int8)
import Frames.Time.Chicago.Columns
import Frames.Time.Chicago.TimeIn
import Data.Time
import Data.String (IsString(..))
import qualified Data.HashSet as HS
import Data.Foldable as F
import Control.Monad.Primitive (PrimMonad)
import Frames.InCore (RecVec, VectorFor(..))
import qualified Data.Vector as VB
import Data.Time.Lens

-- An en passant Default class
class Default a where
  def :: a

instance Default (s :-> Int) where def = Col 0
instance Default (s :-> Text) where def = Col mempty
instance Default (s :-> Double) where def = Col 0.0
instance Default (s :-> Bool) where def = Col False

instance Default (s :-> Chicago) where def = Col (Chicago (TimeIn "America/Chicago"))
instance (IsString ZonedTime) where fromString = isStringZonedTime

-- We can write instances for /all/ 'Rec' values.
instance (Applicative f, LAll Default ts, RecApplicative ts)
  => Default (Rec f ts) where
  def = reifyDict [pr|Default|] (pure def)


-- TODO reconsider whether using the ord instance of the raw LocalTime with no timezone is okay or not
instance Ord Chicago where
    (Chicago (TimeIn (ZonedTime lt1 _))) `compare` (Chicago (TimeIn (ZonedTime lt2 _))) = lt1 `compare` lt2

type instance VectorFor Chicago = VB.Vector

defaultingProducer :: ( ReadRec rs
                      , RecApplicative rs
                      , MonadIO m
                      , LAll Default rs
                      ) => FilePath -> String -> Producer (Rec Identity rs) m ()
defaultingProducer fp label = readTableMaybe fp >-> P.map (holeFiller label)

holeFiller :: forall ty.
              ( LAll Default ty
              , RecApplicative ty
              ) => String -> Rec Maybe ty -> Rec Identity ty
holeFiller label rec1 = let fromJust = fromMaybe (error $ label ++ " failure")
                            firstRec1 :: Rec First ty
                            firstRec1 = rmap First rec1
                            concattedStuff :: Rec First ty
                            concattedStuff = rapply (rmap (Lift . flip (<>)) def) firstRec1
                            firsts :: Rec Maybe ty
                            firsts = rmap getFirst concattedStuff
                            recMaybed :: Maybe (Rec Identity ty)
                            recMaybed = recMaybe firsts
                        in fromJust recMaybed

-- TODO document how this works
-- does it take in a possibly faulty data source and then print out any rows from the source of truth
-- which the possibly faulty data source does not have?
findMissingRowsOn :: forall (checkRec :: [*]) (outRec :: [*])  (monad :: * -> *) (key :: *).
                     ( Monad monad
                     , Ord key
                     , Show key
                     ) =>
                     Getting key (Rec Identity checkRec) key -- lens
                  -> Getting key (Rec Identity outRec) key -- lens
                  -> Producer (Record checkRec) monad ()     -- checkProducer
                  -> monad (Pipe (Record outRec) (Record outRec) monad ())
findMissingRowsOn lens1 lens2 checkProducer = do
  keyMap <- P.fold (\m r -> M.insert (view lens1 (r :: Record checkRec)) (0 :: Int8) m) M.empty id checkProducer
  pure $ P.filter (\(r :: Record rec2) -> M.notMember (view lens2 (r :: Record outRec))  keyMap)



-- | function alias to make creating a Day more concise
dt = fromGregorian

-- | extracts the zoned time out of a Chicago type
chicagoToZoned = (\(Chicago (TimeIn zt)) -> zt)

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


-- | Returns a HashSet of unique values in a row according to a lens and given rowProducer
distinctOn lens1 rowProducer = do
  P.fold
    (\accumSet currentRow -> HS.insert (view lens1 currentRow) accumSet) -- what we build up the set with
    HS.empty -- initial value
    id -- we just want to return the value we inserted, nothing more
    rowProducer -- our rowProducer, in this specific case it is just `rows`

-- | Performs an inner join and keeps any duplicate column
-- Recommend keeping columns in producers disjoint because accessing
-- anything but the leftmost duplicate column could prove difficult.
-- see: https://github.com/VinylRecords/Vinyl/issues/55#issuecomment-269891633
innerJoin
  :: (RecVec (as ++ bs), PrimMonad m, Ord k) =>
     Producer (Rec Identity as) m ()
     -> Getting k (Rec Identity as) k
     -> Producer (Rec Identity bs) m ()
     -> Getting k (Rec Identity bs) k
     -> m (FrameRec (as ++ bs))
innerJoin leftProducer leftLens rightProducer rightLens = do
  -- build a Map with values from the left producer
  leftKeyMap <- P.fold (\m r -> M.insert (view leftLens r) r m) M.empty id leftProducer
  inCoreAoS (rightProducer
             >-> P.filter (\r -> M.member (view rightLens r) leftKeyMap)
             >-> P.map (\row -> do
                          case M.lookup (view rightLens row) leftKeyMap of
                            Just leftRow -> do
                              rappend leftRow row
                            Nothing -> error "this shouldn't happen"
                           )
            )
