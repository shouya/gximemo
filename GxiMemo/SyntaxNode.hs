module GxiMemo.SyntaxNode where


import Control.Monad


data RepetitionTime = Integer Integer | Infinity
                    deriving (Show)

data Expression = Atom { atom :: String }
                | Rule  { ruleName :: String }
                | RuleX { ruleXName :: String }
                | Choice { choice :: [Expression] }
                | Sequence { sequence :: [Expression] }
                | Repetition { repItem     :: Expression
                             , minTime     :: RepetitionTime
                             , maxTime     :: RepetitionTime
                             }
                | NegativeLookahead { nlItem :: Expression }
                | PositiveLookahead { plItem :: Expression }
                deriving (Show)

data MatchData = AtomM { atomM :: String }
               | RuleM { ruleNameM :: String
                       , ruleBodyM :: MatchData
                       }
               | RuleM' { ruleNameM' :: String
                        , ruleBodyM' :: MatchData
                        }
               | ChoiceM { choiceM :: MatchData }
               | SequenceM { sequenceM :: [MatchData] }
               | RepetitionM { repetitionM :: [MatchData] }
               | NegativeLookaheadM
               | PositiveLookaheadM
               | NothingM
               deriving (Show)


manyTimes a = Repetition a (Integer 1) Infinity    -- +
anyTimes  a = Repetition a (Integer 0) Infinity    -- *
optional  a = Repetition a (Integer 0) (Integer 1) -- ?


type RuleTable = [(String, Expression)]

class Matchable a where
  match :: String -> a -> RuleTable -> Maybe (String, MatchData)


matchRule resultT =  \str r t ->
  lookup r t >>= \rule ->
  match str rule t >>= \(s, m) ->
  return (s, resultT r m)

instance Matchable Expression where
  match str (Atom s) _ = if s == take (length s) str
                         then return (s, AtomM s)
                         else Nothing


  match str (Rule r) t  = (matchRule RuleM)  str r t
  match str (RuleX r) t = (matchRule RuleM') str r t


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


----------------------------------------------
-- data MatchData = AtomM String            --
--                | RuleM String MatchData  --
--                | ChoiceM MatchData       --
--                | SequenceM [MatchData]   --
--                | RepetitionM [MatchData] --
--                | NegativeLookaheadM      --
--                | PositiveLookaheadM      --
----------------------------------------------



isAtomM (AtomM _) = True
isAtomM _         = False
isNothing NothingM = True
isNothing _        = False

simplify :: MatchData -> MatchData
simplify (SequenceM xs)
  | length xs' == 0 = NothingM
  | length xs' == 1  = head xs'
  | all isAtomM xs' = foldl1 (\(AtomM a) (AtomM b) -> AtomM (a ++ b)) xs'
  | otherwise       = SequenceM xs'
  where xs' = filter (not . isNothing) $ map simplify xs

simplify (RuleM name dat) = RuleM name (simplify dat)
simplify (RuleM' name dat) = simplify dat
simplify (ChoiceM x) = simplify x
simplify (RepetitionM xs)
  | length xs' == 0 = NothingM
  | length xs' == 1 = head xs'
  | all isAtomM xs' = foldl1 (\(AtomM a) (AtomM b) -> AtomM (a ++ b)) xs'
  | otherwise       = RepetitionM xs'
  where xs' = filter (not . isNothing) $ map simplify xs
simplify x = x
