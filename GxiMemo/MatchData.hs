
module GxiMemo.MatchData where

import Control.Monad

-- type Offset = Integer
-- type StartPosition = Integer
-- type EndPosition = Integer
data MatchData = MatchData { residual    :: String
                           , matchLength :: Integer
                           , offset      :: Integer
                           } deriving (Show)
type MatchResult = Maybe MatchData

matchLen :: MatchData -> Integer -> MatchData
matchLen m l = MatchData (drop (fromIntegral l) $ residual m) l (offset m + l)
