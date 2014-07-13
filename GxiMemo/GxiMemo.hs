
module GxiMemo where

import Parser

import Control.Monad.State
import Data.Map as M hiding (map)


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


parse :: String -> RuleMap -> String -> Maybe MatchData
parse str rm start = evalState (parseI $ main) $ ParsingState str rm
  where main = rm ! start


debugParse :: String -> IO (Maybe MatchData, ParsingState)
debugParse start = readFile "Parser.memo" >>= \str ->
  return $ runState (parseI $ main) $ ParsingState str rm
  where rm = fromList rules
        main = rm ! start


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
