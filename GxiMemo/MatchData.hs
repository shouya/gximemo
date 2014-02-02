
module GxiMemo.MatchData where

import Control.Monad

-- type Offset = Integer
-- type StartPosition = Integer
-- type EndPosition = Integer
data MatchData a = MatchData { residual    :: String
                             , matchLength :: Integer
                             , offset      :: Integer
                             , matched     :: a
--                           , isExhausted :: Bool
                           } deriving (Show)
type MatchResult = Maybe MatchData

matchLen :: MatchData -> Integer -> MatchData
matchLen m l md = MatchData (drop (fromIntegral l) $ residual m)
                            l (offset m + l) md
