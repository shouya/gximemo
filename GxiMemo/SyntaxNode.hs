
module GxiMemo.SyntaxNode where

import Control.Monad (liftM)

-- import GxiMemo.MatchData

data RepetitionTime = Integer Integer | Infinity
                    deriving (Show)

data Expression a = Atom { atom :: String }
                  | Symbol { symName :: String }
                  | Choice { choice :: [Expression a] }
                  | Sequence { sequence :: [Expression a] }
                  | Repetition { repItem     :: a
                               , minTime     :: RepetitionTime
                               , maxTime     :: RepetitionTime
                               }
                  | NegativeLookahead { nlItem :: a }
                  | PositiveLookahead { plItem :: a }
                  deriving (Show)

data MatchableTerminal = MatchableTerminal


class Matchable a where
  match :: String -> a -> Maybe String
  (=~)  :: String -> a -> Maybe String

  str =~ pat    = match str pat
  match str pat = str =~ pat

instance (Matchable a) => Matchable (Expression a) where
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

instance Matchable MatchableTerminal where
