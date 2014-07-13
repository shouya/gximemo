{-# LANGUAGE TemplateHaskell #-}

module GxiMemo.Parser where

import Control.Monad
import Control.Monad.State
import Control.Lens

import Data.Map as M hiding (map)


type RuleName = String
type RulePair = (RuleName, Pattern)
type RuleMap = M.Map RuleName Pattern

data RepTime = RTInt Integer -- n-times
             | RTInf         -- infinity
             deriving (Show)

manyTimes :: Pattern -> Pattern
manyTimes a = Repetition a (RTInt 1) RTInf     -- +
anyTimes :: Pattern -> Pattern
anyTimes  a = Repetition a (RTInt 0) RTInf     -- *
optional :: Pattern -> Pattern
optional  a = Repetition a (RTInt 0) (RTInt 1) -- ?

rtZerop :: RepTime -> Bool
rtZerop (RTInt 0) = True
rtZerop _ = False

rtPred :: RepTime -> RepTime
rtPred (RTInt 0) = RTInt 0
rtPred (RTInt n) = RTInt (n - 1)
rtPred RTInf = RTInf


data Pattern = Atom String
             | Rule String
             | RuleX String  -- rule that only do matching but ignoring the result
             | Repetition Pattern RepTime RepTime -- pat, minTime, maxTime
             | Choice [Pattern]
             | Sequence [Pattern]
             | NegativeLookahead Pattern
             | PositiveLookahead Pattern
             deriving (Show)


rules :: [RulePair]
rules =
  [("main", Sequence [RuleX "spaces", Rule "rule",
                      anyTimes (Sequence [manyTimes (RuleX "newline"),
                                          Rule "rule"])])
  ,("space", Choice (map Atom [" ", "\t", "\r\n", "\r", "\n"]))
  ,("space_nonl", Choice (map Atom [" ", "\t"]))
  ,("spaces", anyTimes (RuleX "space"))
  ,("spaces_nonl", anyTimes (RuleX "space_nonl"))
  ,("newline", Choice (map Atom ["\r\n", "\r", "\n"]))
  ,("alpha", Choice (map (Atom . (:[])) (['a'..'z'] ++ ['A'..'Z'])))
  ,("alpha_underscore", Choice [RuleX "alpha", Atom "_"])
  ,("digit", Choice (map (Atom . (:[])) ['0'..'9']))
  ,("integer", manyTimes (RuleX "digit"))
  ,("token", Sequence [RuleX "alpha_underscore",
                       anyTimes (Choice [RuleX "alpha_underscore",
                                         RuleX "digit"])])
  ,("char", Choice ([RuleX "alpha", RuleX "digit"] ++
                    (map (Atom . (:[])) " ~`!@#$%^&*()-_=+[]{}:;'<>?,./|") ++
                    [Atom "\\\\", Atom "\\\"",
                     Sequence [Atom "\\", RuleX "alpha"]]))


  ,("rule", Sequence [RuleX "spaces", Rule "token",
                      RuleX "spaces", Atom "=",
                      RuleX "spaces", Rule "choice", RuleX "spaces_nonl"])
  ,("choice", Sequence [RuleX "sequence",
                        anyTimes (Sequence [RuleX "spaces", RuleX "|",
                                            RuleX "spaces", Rule "sequence"])])
  ,("sequence", Sequence [Rule "repetition",
                          anyTimes (Sequence [RuleX "spaces_nonl",
                                              Rule "repetition"])])
  ,("repetition", Sequence [Rule "expression",
                            optional $
                            Choice [Atom "?", Atom "+", Atom "*",
                                    Sequence [Atom "{",
                                              optional (RuleX "number"),
                                              Atom ",",
                                              optional (RuleX "number"),
                                              Atom "}"]]])
  ,("expression", Choice [Sequence [Atom "(", RuleX "spaces", Rule "choice",
                                    RuleX "spaces", Atom ")"],
                          Rule "string",
                          Rule "token",
                          Rule "token_omitted",
                          Rule "pos_lookahead",
                          Rule "neg_lookahead"])
  ,("token_omitted", Sequence [Atom "@", RuleX "token"])
  ,("string", Sequence [Atom "\"", anyTimes (RuleX "char"), Atom "\""])
  ,("pos_lookahead", Sequence [Atom "=", Rule "expression"])
  ,("pos_lookahead", Sequence [Atom "!", Rule "expression"])
  ]


data MatchData = MAtom String
               | MList [MatchData]
               | MPair (RuleName, MatchData)

instance Show MatchData where
  show (MPair (name,md)) = "{" ++ (show md) ++ "}:" ++ name
  show (MList xs) = join $ map show xs
  show (MAtom x)  = x

mToString :: MatchData -> String
mToString (MPair (_,md)) = mToString md
mToString (MList xs) = join $ map mToString xs
mToString (MAtom s) = s

-- flattening a match data
mAtomify :: MatchData -> MatchData      -- always return (MAtom x)
mAtomify = MAtom . mToString

data ParsingState = ParsingState { _restString :: String,
                                   _ruleMap    :: RuleMap
                                 }
makeLenses ''ParsingState


type ParseState = State ParsingState
type ParseResult = ParseState (Maybe MatchData)


getRestString :: ParseState String
getRestString = get >>= return . view restString
putRestString :: String -> ParseState ()
putRestString st = modify (restString .~ st)

-- read only state
getRuleMap :: ParseState RuleMap
getRuleMap = get >>= return . view ruleMap

lookupRule :: RuleName -> ParseState (Maybe Pattern)
lookupRule name = getRuleMap >>= return . M.lookup name


parseI :: Pattern -> ParseResult

parseI (Atom x) = do
  rest <- getRestString
  len <- return $ length x
  if (take len rest) == x
  then do putRestString (drop len rest)
          return $ Just $ MAtom x
  else    return Nothing

parseI (Rule name) = do
  pat' <- lookupRule name
  case pat' of
    Nothing -> return Nothing
    Just pat -> do
      result <- parseI pat
      case result of
        Nothing -> return Nothing
        Just m  -> return $ Just $ MPair (name, m)

parseI (RuleX name) = do
  pat' <- lookupRule name
  case pat' of
    Nothing -> return Nothing
    Just pat -> do
      result <- parseI pat
      case result of
        Nothing -> return Nothing
        Just m  -> return $ Just $ mAtomify m

parseI (Repetition pat l' u') =
  if rtZerop u' then return $ Just $ MList []
  else do
    m' <- parseI pat
    case m' of
      Nothing -> if rtZerop l' then return (Just $ MList [])
                 else return Nothing               -- error
      Just m  -> do
        ms' <- parseI (Repetition pat (rtPred l') (rtPred u'))
        case ms' of
          Nothing         -> return Nothing
          Just (MList ms) -> return $ Just $ MList (m : ms)
          Just _          -> error "impossible"


parseI (Choice []) = return Nothing
parseI (Choice [p]) = do
  m' <- parseI p
  case m' of
    Nothing -> return Nothing
    Just _  -> return m'

parseI (Choice (p:ps)) = do
  oldST <- get
  m' <- parseI p
  case m' of
    Just _  -> return m'
    Nothing -> do
      put oldST
      parseI (Choice ps)

parseI (Sequence []) = return $ Just $ MList []
parseI (Sequence (p:ps)) = do
  m' <- parseI p
  case m' of
    Nothing -> return Nothing
    Just m  -> do
      ms' <- parseI (Sequence ps)
      case ms' of
        Nothing   -> return Nothing
        Just ms'' -> case ms'' of
          MList ms -> return $ Just $ MList (m:ms)
          _        -> error "impossible!"


parseI (NegativeLookahead p) = do
  oldST <- get
  m' <- parseI p
  put oldST
  case m' of
    Nothing -> return $ Just $ MAtom ""
    Just _  -> return Nothing

parseI (PositiveLookahead p) = do
  oldST <- get
  m' <- parseI p
  put oldST
  case m' of
    Just _   -> return $ Just $ MAtom ""
    Nothing  -> return Nothing




{-
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
-}


parse :: String -> RuleMap -> String -> Maybe MatchData
parse str rm start = evalState (parseI $ main) $ ParsingState str rm
  where main = rm ! start


convertToGxiMemoPattern :: MatchData -> Maybe Pattern
convertToGxiMemoPattern = undefined

extract  :: RuleName -> MatchData -> Maybe MatchData
extract = undefined
extracts :: RuleName -> MatchData -> Maybe [MatchData]
extracts = undefined

parseToRuleList :: String -> Maybe [Pattern]
parseToRuleList str = do
  main <- parse str (fromList rules) "main" >>= extract "main"
  rawrules <- extracts "rule" main
  rules' <- mapM convertToGxiMemoPattern rawrules
  return rules'
