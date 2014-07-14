-- -*- coding: utf-8 -*-
module GxiMemo where

import Parser

import Control.Monad.State
import Data.List
import Data.Map as M hiding (map, filter)


rules :: [RulePair]
rules =
  [("main", Sequence [RuleX "spaces", Rule "rule",
                      anyTimes (Sequence [manyTimes (RuleX "newline"),
                                          Rule "rule"])])
  ,("space", Choice (map Atom [" ", "\t", "\r\n", "\r", "\n"]))
  ,("space_nonl", Choice (map Atom [" ", "\t"]))
  ,("spaces", anyTimes (RuleX "space"))
  ,("spaces_nonl", anyTimes (Rule "space_nonl"))
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


  ,("rule", Sequence [Rule "token",
                      RuleX "spaces", Atom "=",
                      RuleX "spaces", Rule "choice", RuleX "spaces_nonl"])
  ,("choice", Sequence [Rule "sequence",
                        anyTimes (Sequence [RuleX "spaces", Atom "|",
                                            RuleX "spaces", Rule "sequence"])])
  ,("sequence", Sequence [Rule "repetition",
                          anyTimes (Sequence [RuleX "spaces_nonl",
                                              Rule "repetition"])])
  ,("rep_optional", Atom "?")
  ,("rep_manytimes", Atom "+")
  ,("rep_anytimes", Atom "*")
  ,("rep_mintime", RuleX "number")
  ,("rep_maxtime", RuleX "number")
  ,("rep_mark", Choice [Rule "rep_optional",
                        Rule "rep_manytimes",
                        Rule "rep_anytimes",
                        Sequence [Atom "{",
                                  optional (Rule "rep_mintime"),
                                  Atom ",",
                                  optional (Rule "rep_maxtime"),
                                  Atom "}"]])

  ,("repetition", Sequence [Rule "expression",
                            optional $ Rule "rep_mark"])
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


parse :: String -> RuleMap -> String -> Maybe MatchData
parse str rm start = evalState (parseI $ main) $ ParsingState str rm
  where main = rm ! start


simplify :: MatchData -> MatchData

simplify (MList (xs)) = xs'''
  where notnul x = mToString x /= ""
        xs' = map simplify xs
        xs'' = filter notnul xs'
        xs''' = case length xs'' of
          0 -> MAtom ""
          1 -> head xs''
          _ -> MList xs''

simplify (MPair (n,m)) = MPair (n,simplify m)
simplify x = x


penetrateSubstitute :: String -> MatchData -> MatchData
penetrateSubstitute name xs =  case xs of
  MList xs' -> if length xs'' == 1 then head xs''
               else MPair (name, MList xs'')
    where xs'' = map simplifyGM xs'
  _         -> simplifyGM xs


simplifyGM :: MatchData -> MatchData
simplifyGM (MList xs) = MList $ map simplifyGM xs

simplifyGM (MPair ("choice",xs)) = penetrateSubstitute "choice" xs
simplifyGM (MPair ("sequence",xs)) = penetrateSubstitute "sequence" xs
simplifyGM (MPair ("repetition",xs)) = penetrateSubstitute "repetition" xs
simplifyGM (MPair ("expression",xs)) = penetrateSubstitute "expression" xs
simplifyGM (MPair ("rep_mark",xs)) = penetrateSubstitute "rep_mark" xs

simplifyGM (MPair (n,x)) = MPair (n,simplifyGM x)
simplifyGM x = x

-- debugParse :: String -> IO (Maybe MatchData, ParsingState)
debugParse start = readFile "Parser.memo" >>= \str ->
  return $ ((evalState (parseI $ main) $ ParsingState str rm) >>=
                 (return . simplifyGM . simplify))
  where rm = fromList rules
        main = rm ! start

debugConvert :: IO ()
debugConvert = readFile "Parser.memo" >>= \str ->
  case parseToRuleList str of
    Just m  -> print $ intercalate "\n\n" $ map show m
    Nothing -> print "Fork! 怎麼又解析出錯了!"


extract  :: RuleName -> MatchData -> Maybe MatchData
extract name (MList xs) = find foo xs
  where foo (MPair (name',_)) = if name == name'
                                then True
                                else False
        foo _ = False
extract _ _ = error "invalid"

extractString :: RuleName -> MatchData -> Maybe String
extractString name m = extract name m >>= return . mToString


toPatternPair :: MatchData -> Maybe RulePair
toPatternPair (MPair (_,m)) = do
  name <- extract "token" m >>= return . mToString . snd . getMPair
  toPattern m >>= \pat -> return (name, pat)
toPatternPair x = toPattern x >>= \x' -> Just $ ("error", x')
-- impossible: should be Nothing

toPattern :: MatchData -> Maybe Pattern
toPattern (MPair ("string", str)) = return $ Atom $ tail $ init $ mToString str
toPattern (MPair ("token", str)) = return $ Rule $ mToString $ str
toPattern (MPair ("token_omitted", str)) =
  return $ RuleX $ tail $ mToString $ str                    -- skip the '@'
toPattern (MPair ("choice", xs)) =
  (mapM toPattern $ filter isMPair $ getMList xs) >>= return . Choice
toPattern (MPair ("sequence", xs)) =
  (mapM toPattern $ filter isMPair $ getMList xs) >>= return . Sequence
toPattern x = Just $ Atom $ mInspect x
-- toPattern (MPair ("repetition", xs)) =
--      Sequence $ map toPattern $ filter isMPair xs



parseToRuleList :: String -> Maybe [RulePair]
parseToRuleList str = do
  rawrules <- parse str (fromList rules) "main" >>= return . simplifyGM . simplify
  case rawrules of
    MList xs -> mapM toPatternPair xs
    _        -> Nothing
