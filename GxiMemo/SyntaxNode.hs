
module GxiMemo.SyntaxNode where

import Control.Monad (liftM)

-- import GxiMemo.MatchData

data RepetitionTime = Integer Integer | Infinity
                    deriving (Show)

data Expression = Atom { atom :: String }
                | Rule { ruleName :: String }
                | Choice { choice :: [Expression] }
                | Sequence { sequence :: [Expression] }
                | Repetition { repItem     :: Expression
                             , minTime     :: RepetitionTime
                             , maxTime     :: RepetitionTime
                             }
                | NegativeLookahead { nlItem :: Expression }
                | PositiveLookahead { plItem :: Expression }
                deriving (Show)


manyTimes a = Repetition a (Integer 1) Infinity    -- +
anyTimes  a = Repetition a (Integer 0) Infinity    -- *
optional  a = Repetition a (Integer 0) (Integer 1) -- ?

class Matchable a where
  match :: String -> a -> Maybe String
  (=~)  :: String -> a -> Maybe String

  str =~ pat    = match str pat
  match str pat = str =~ pat



instance Matchable Expression where
  str =~ Atom s = if s == take (length s) str
                  then return s
                  else Nothing

  str =~ Choice (x:xs) = case (str =~ x) of
    Just a  -> return a
    Nothing -> str =~ (Choice xs)
  str =~ Choice _      = Nothing


  str =~ Sequence (x:xs) = (str =~ x) >>= \a ->
    str =~ Sequence xs >>= \b -> return (a ++ b)
  str =~ Sequence _      = Just ""


  str =~ (Repetition x (Integer l) Infinity)    = case str =~ x of
    Just p  -> matchresult >>= \q -> return (p ++ q)
      where matchresult = (drop (length p) str) =~
                          (Repetition x (Integer (l-1)) Infinity)
    Nothing -> if l > 0 then Nothing
               else return ""

  str =~ (Repetition x (Integer l) (Integer u)) = case (str =~ x) of
    Just p  -> if u <= 1 then return p
               else matchresult >>= \q -> return (p ++ q)
      where matchresult = (drop (length p) str) =~
                          (Repetition x (Integer (l-1)) (Integer (u-1)))
    Nothing -> if l > 0 then Nothing
               else return ""

  str =~ (NegativeLookahead x) = case str =~ x of
    Just _  -> Nothing
    Nothing -> Just ""
  str =~ (PositiveLookahead x) = (str =~ x) >>= \_ -> return ""
