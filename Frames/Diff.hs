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
                   , distinctOn
                   , innerJoin
                   -- , innerJoin'
                   , takeFrame
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
import qualified Pipes as P
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M
import qualified Data.Text.Format as T
import Data.Int (Int8)
import Data.Time
import Data.Proxy
import qualified Data.HashSet as HS
import Data.Foldable as F
import Control.Monad.Primitive (PrimMonad)
import Frames.InCore (RecVec, VectorFor(..))
import Frames.Default


-- We can write instances for /all/ 'Rec' values.
instance (Applicative f, LAll Default ts, RecApplicative ts)
  => Default (Rec f ts) where
  def = reifyDict [pr|Default|] (pure def)

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
-- innerJoin :: (MonadIO m, Ord k) =>
--              Producer (Rec f leftRows) IO ()  -- leftProducer
--           -> Getting k (Rec f leftRows) k     -- leftProducer lens
--           -> Producer (Rec f rightRows) IO () -- rightProducer
--           -> Getting k (Rec f rightRows) k    -- rightProducer lens
--           -> m (P.Proxy P.X () () (Rec f (leftRows ++ rightRows)) IO ())
-- innerJoin leftProducer leftLens rightProducer rightLens = do
--   leftProducerLen <- P.liftIO $ P.length leftProducer
--   rightProducerLen <- P.liftIO $ P.length rightProducer

--   let curProducer = case rightProducerLen < leftProducerLen of
--                       True -> rightProducer
--                       -- False -> leftProducer

--   let curKeymapProducer = case rightProducerLen < leftProducerLen of
--                             True -> leftProducer
--                             -- False -> rightProducer

--   let curLensLookup = case rightProducerLen < leftProducerLen of
--                   True -> rightLens
--                   -- False -> leftLens

--   let curLensInsert = case rightProducerLen < leftProducerLen of
--                   True -> leftLens
--                   -- False -> rightLens


--   let appender km row = case rightProducerLen < leftProducerLen of
--                           True -> case M.lookup (view curLensLookup row) km of
--                                      Just otherRow -> pure $ rappend otherRow row
--                                      Nothing -> P.mzero
--                           -- False -> case M.lookup (view curLensLookup row) km of
--                           --            Just otherRow -> pure $ rappend row otherRow
--                           --            Nothing -> P.mzero

--   keyMap <- P.liftIO $ P.fold (\m r -> M.insert (view curLensInsert r) r m) M.empty id curKeymapProducer

--   pure $ curProducer >-> P.mapM (\r -> appender keyMap r)

innerJoinHelper
  :: (RecVec (as ++ bs), PrimMonad m, Ord k) =>
     Producer (Rec Identity as) m ()
     -> Getting k (Rec Identity as) k
     -> Producer (Rec Identity bs) m ()
     -> Getting k (Rec Identity bs) k
     -> m (FrameRec (as ++ bs))
innerJoinHelper leftProducer leftLens rightProducer rightLens = do
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

innerJoinHelperRight
  :: (RecVec (bs ++ as), PrimMonad m, Ord k) =>
     Producer (Rec Identity as) m ()
     -> Getting k (Rec Identity as) k
     -> Producer (Rec Identity bs) m ()
     -> Getting k (Rec Identity bs) k
     -> m (FrameRec (bs ++ as))
innerJoinHelperRight leftProducer leftLens rightProducer rightLens = do
  -- build a Map with values from the left producer
  leftKeyMap <- P.fold (\m r -> M.insert (view leftLens r) r m) M.empty id leftProducer
  inCoreAoS (rightProducer
             >-> P.filter (\r -> M.member (view rightLens r) leftKeyMap)
             >-> P.map (\row -> do
                          case M.lookup (view rightLens row) leftKeyMap of
                            Just leftRow -> do
                              rappend row leftRow 
                            Nothing -> error "this shouldn't happen"
                           )
            )

innerJoin leftProducer leftLens rightProducer rightLens = do
  rightLen <- P.length leftProducer
  leftLen <- P.length rightProducer

  case leftLen > rightLen of
    True -> (innerJoinHelper leftProducer leftLens rightProducer rightLens)
    False -> (innerJoinHelperRight rightProducer rightLens leftProducer leftLens)

type UserId = "userId" :-> Int
type Simple1 = Record '["auto" :-> Int, UserId]
type Simple2 = Record '[UserId, "uslessCol" :-> Text]
userId ::
  forall f rs. (Functor f, UserId ∈ rs) =>
  (Int -> f Int) -> Record rs -> f (Record rs)
userId = rlens (Proxy :: Proxy UserId)

simple1 :: [Simple1]
simple1 = [ 0 &: 100 &: Nil
          , 1 &: 100 &: Nil
          , 2 &: 100 &: Nil
          , 3 &: 100 &: Nil
          ]

simple2 :: [Simple2]
simple2 = [ 100 &: "usless" &: Nil]


-- expect to get 4 rows regardless of whether simple1 or simple2 is on left side of join

-- runEffect $ innerJoin' (P.each simple1) userId (P.each simple2) userId  >-> P.print

-- | turns a Frame into a list and takes n elements from it
-- TODO is there a more efficient way?? Subset slicing works in this case.
-- TODO is there a more efficient way to use the List api on Frames?
takeFrame n = take n . F.toList
