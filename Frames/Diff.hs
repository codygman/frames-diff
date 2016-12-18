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
module Frames.Diff (defaultingProducer, findMissingRowsOn) where

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
import qualified Data.Map as M
import qualified Data.Text.Format as T

-- An en passant Default class
class Default a where
  def :: a

instance Default (s :-> Int) where def = Col 0
instance Default (s :-> Text) where def = Col mempty
instance Default (s :-> Double) where def = Col 0.0
instance Default (s :-> Bool) where def = Col False

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
  keyMap <- P.fold (\m r -> M.insert (view lens1 (r :: Record checkRec)) 0 m) M.empty id checkProducer
  pure $ P.filter (\(r :: Record rec2) -> M.notMember (view lens2 (r :: Record outRec))  keyMap)
