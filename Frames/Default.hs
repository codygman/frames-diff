{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Frames.Default where

import Frames hiding ((:&))

-- An en passant Default class
class Default a where
  def :: a

instance Default (s :-> Int) where def = Col 0
instance Default (s :-> Text) where def = Col mempty
instance Default (s :-> Double) where def = Col 0.0
instance Default (s :-> Bool) where def = Col False

