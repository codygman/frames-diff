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
module Frames.Diff (defaultingProducer, findMissingRowsOn, Default(..)) where

import Frames hiding ((:&))
import Frames.CSV (RowGen(..), ReadRec)
import Control.Monad.IO.Class (MonadIO)
import Data.Vinyl (Rec, RecApplicative(rpure), rmap, rapply)
import Data.Vinyl.Functor (Lift(..), Identity(..))
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
import Frames.Time.Columns
import Frames.Time.TimeIn
import Data.Time
import Data.String (IsString(..))

-- An en passant Default class
class Default a where
  def :: a

instance Default (s :-> Int) where def = Col 0
instance Default (s :-> Text) where def = Col mempty
instance Default (s :-> Double) where def = Col 0.0
instance Default (s :-> Bool) where def = Col False

instance Default (s :-> Chicago) where def = Col (Chicago (TimeIn "America/Chicago"))
instance (IsString UTCTime) where fromString = isStringUtcTime

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


-- normalized >-> distinctOn DateCol

-- distinctOn :: forall (checkRec :: [*]) (outRec :: [*]) (keysRec :: [*]) (monad :: * -> *) (key :: *).
--                      ( Monad monad
--                      , Ord key
--                      , Show key
--                      ) =>
--                      Getting key (Rec Identity checkRec) key -- lens
--                   -> Producer (Record checkRec) monad ()     -- checkProducer
--                   -> monad (Pipe (Record keysRec) (Record keysRec) monad ())
-- distinctOn lens1 checkProducer = do
--     distinctValuesMap <- P.fold (\m r -> M.insert (view lens1 (r :: Record checkRec)) (0 :: Int8) m) M.empty id checkProducer
--     pure $ _ (head  $ M.keys distinctValuesMap)
  -- distinctValues are above.. two options:
  -- need to create a record with those distinctValues
  -- undefined
  -- pure $ P.filter (\(r :: Record rec2) -> M.notMember (view lens2 (r :: Record outRec))  keyMap)
  -- M.keys keyMap

-- idea for implementing joinOn
-- [23:31] <Cale> codygman: Perhaps ListT will do what you want
-- [23:32] <Cale> codygman: You could write something like  enumerate $ liftM2 (,) (Select producer1) (Select producer2)  to get a full join
