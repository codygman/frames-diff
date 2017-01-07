{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Frames.ColumnTypeable 
import Frames.Time.LocalTime
import qualified Data.Text as T
import Data.Thyme
import Control.Monad

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- testParse1e6 :: MonadPlus m => [m (Parsed LocalTime)]
testParse1e6 :: Int -> [Maybe (Parsed LocalTime)]
testParse1e6 n = map parse (Prelude.replicate n ("2016-05-03 00:00:00 +0006 CST" :: T.Text))

-- Our benchmark harness.
main = defaultMain [
  bgroup "date parsing" [ bench "1000"  $ whnf testParse1e6 1000
                        , bench "10000"  $ whnf testParse1e6 10000
                        , bench "100000"  $ whnf testParse1e6 10000
                        , bench "1000000"  $ whnf testParse1e6 100000
                        ]
  ]