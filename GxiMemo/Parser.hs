{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Control.Monad
import Control.Monad.State
import Control.Lens

import Data.List
import Data.Map as M hiding (map)


type RuleName = String
type RulePair = (RuleName, Pattern)
type RuleMap = M.Map RuleName Pattern

data RepTime = RTInt Integer -- n-times
             | RTInf         -- infinity
             deriving (Show,Eq)

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
               -- TODO: RepetitionL : Lazy repetition
             | Repetition Pattern RepTime RepTime -- pat, minTime, maxTime
             | Choice [Pattern]
             | Sequence [Pattern]
             | NegativeLookahead Pattern
             | PositiveLookahead Pattern
             deriving (Show,Eq)
{-
instance Show Pattern where
  show (Atom x) = x
  show (Rule x) = '@' : x
  show (RuleX x) = "@@" ++ x
  show (Repetition p a b) = (show p) ++ (mark a b)
    where mark (RTInt 0) (RTInt 1) = "?"
          mark (RTInt 1) RTInf     = "+"
          mark (RTInt 0) RTInf     = "*"
          mark (RTInt l) (RTInt u) = "{" ++ show l ++ "," ++ show u ++ "}"
          mark _ _ = undefined
  show _ = ""
-}


data MatchData = MAtom String
               | MList [MatchData]
               | MPair (RuleName, MatchData)
               deriving (Eq)

instance Show MatchData where
  show (MPair (name,md)) = "{" ++ (show md) ++ "}:" ++ name
  show (MList xs) = join $ map show xs
  show (MAtom x)  = x

isMPair :: MatchData -> Bool
isMPair (MPair (_,_)) = True
isMPair _ = False

getMPair :: MatchData -> (RuleName, MatchData)
getMPair (MPair (r,m)) = (r,m)
getMPair _ = error "not a MPair"


isMList :: MatchData -> Bool
isMList (MList _) = True
isMList _ = False

getMList :: MatchData -> [MatchData]
getMList (MList xs) = xs
--DEUBG:
getMList a = error (mInspect a ++ " is not a MList")
--getMList _ = error "not a MList"

isMAtom :: MatchData -> Bool
isMAtom (MAtom _) = True
isMAtom _ = False

getMAtom :: MatchData -> String
getMAtom (MAtom s) = s
getMAtom _ = error "not a MAtom"


mInspect :: MatchData -> String
mInspect (MPair (n,md)) = n ++ "=>" ++ mInspect md
mInspect (MList xs) = "[" ++ (intercalate "/" $ map mInspect xs) ++ "]"
mInspect (MAtom s) = s


mToString :: MatchData -> String
mToString (MPair (_,md)) = mToString md
mToString (MList xs) = join $ map mToString xs
mToString (MAtom s) = s

-- flattening a match data
mAtomify :: MatchData -> String      -- always return (MAtom x)
mAtomify = mToString

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
        Just m  -> return $ Just $ MAtom $ mAtomify m

parseI (Repetition pat l' u') =
  if rtZerop u' then return $ Just $ MList []
  else do
    oldST <- get
    m' <- parseI pat
    case m' of
      Nothing -> if rtZerop l' then do put oldST
                                       return (Just $ MList [])
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
