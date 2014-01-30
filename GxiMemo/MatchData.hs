
module GxiMemo.MatchData where

type MatchedText = String
data MatchData = Failure
               | Success MatchedText
