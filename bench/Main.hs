{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.API.Time
import qualified Data.Text as T
import Data.Time

main :: IO ()
main =
  defaultMain
  [ bgroup "parseUTC_old" [ bench (T.unpack x) $ nf parseUTC_old x
                          | x <- time_inputs ]
  , bgroup "parseUTC"     [ bench (T.unpack x) $ nf parseUTC x
                          | x <- time_inputs ]
  , bgroup "parseDay"     [ bench (T.unpack x) $ nf parseUTC x
                          | x <- day_inputs ]
  , bgroup "read Day"     [ bench (T.unpack x) $ nf (reads . T.unpack :: T.Text -> [(UTCTime,String)]) x
                          | x <- day_inputs ]
  ]
  where
    time_inputs = [ "nonsense"
                  , "2013-05-27T19:13:50Z"
                  , "1965-03-10T09:23:01.000001Z"
                  , "2999-03-10T09:23"
                  , "2021-03-16T12:24:32+0100"
                  ]

    day_inputs = ["nonsense", "2020-11-11" ]
