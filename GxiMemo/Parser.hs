module GxiMemo.Parser where

import GxiMemo.SyntaxNode

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

parseString str = match str (Rule "main") rules


resolve :: MatchData -> RuleTable
resolve = resolveMain

resolveMain (RuleM "main" (SequenceM xs)) = map resolveRule xs
resolveRule (RuleM "rule" (SequenceM seq)) = (name, body)
  where name = atomM (ruleBodyM (head seq))
        body = resolveExpression (last (init seq))
resolveExpression (RuleM "sequence" (SequenceM seq)) =
  Sequence (
    (resolveExpression (head seq)):
    map (resolveExpression . head . tail . sequenceM) (repetitionM $ last seq))
