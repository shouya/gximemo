
module GxiMemo.SyntaxNode where

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

data MatchData = AtomM String
               | RuleM String MatchData
               | ChoiceM MatchData
               | SequenceM [MatchData]
               | RepetitionM [MatchData]
               | NegativeLookaheadM
               | PositiveLookaheadM
               deriving (Show)


manyTimes a = Repetition a (Integer 1) Infinity    -- +
anyTimes  a = Repetition a (Integer 0) Infinity    -- *
optional  a = Repetition a (Integer 0) (Integer 1) -- ?


type RuleTable = [(String, Expression)]

class Matchable a where
  match :: String -> a -> RuleTable -> Maybe (String, MatchData)


instance Matchable Expression where
  match str (Atom s) _ = if s == take (length s) str
                         then return (s, AtomM s)
                         else Nothing

  match str (Rule r) t = lookup r t >>= \rule ->
    match str rule t >>= \(s, m) ->
    return (s, RuleM r m)

  match str (Choice (x:xs)) t =
    case (match str x t) of
      Just (a,m)  -> return (a, ChoiceM m)
      Nothing -> match str (Choice xs) t
  match str (Choice _) _      = Nothing


  match str (Sequence (x:xs)) t =
    match str x t >>= \(a,m) ->
    match (drop (length a) str) (Sequence xs) t >>= \(b,SequenceM ms) ->
    return (a ++ b, SequenceM (m:ms))
  match str (Sequence _) _      = Just ("", SequenceM [])


  match str (Repetition x (Integer l) Infinity) t =
    case match str x t of
      Just (p,m)  -> matchresult >>= \(q, RepetitionM ms) ->
        return (p ++ q, RepetitionM (m:ms))
        where matchresult = match (drop (length p) str)
                            (Repetition x (Integer (l-1)) Infinity) t
      Nothing -> if l > 0 then Nothing
                 else return ("", RepetitionM [])
  match str (Repetition x (Integer l) (Integer u)) t =
    case match str x t of
      Just (p,m)  -> if u <= 1 then return (p, RepetitionM [m])
                     else matchresult >>= \(q, RepetitionM ms) ->
                     return (p ++ q, RepetitionM (m:ms))
        where matchresult = match (drop (length p) str)
                            (Repetition x (Integer (l-1)) (Integer (u-1))) t
      Nothing -> if l > 0 then Nothing
                 else return ("", RepetitionM [])

  match str (NegativeLookahead x) t = case match str x t of
    Just _  -> Nothing
    Nothing -> Just ("", NegativeLookaheadM)
  match str (PositiveLookahead x) t = (match str x t) >>= \_ ->
    return ("", PositiveLookaheadM)
