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
                      RuleX "spaces", Rule "sequence", RuleX "spaces_nonl"])
  ,("sequence", Sequence [Rule "expression",
                          anyTimes (Sequence [RuleX "spaces_nonl",
                                              Rule "expression"])])
  ,("expression", Sequence [RuleX "expression1", anyTimes (Rule "choice")])
  ,("expression1", Sequence [RuleX "expression2",
                             optional (Rule "repetition")])
  ,("expression2", Choice [Sequence [Atom "(", RuleX "spaces", Rule "sequence",
                                     RuleX "spaces", Atom ")"],
                           Rule "string",
                           Rule "token",
                           Rule "token_omitted",
                           Rule "pos_lookahead",
                           Rule "neg_lookahead"])
  ,("token_omitted", Sequence [Atom "@", RuleX "token"])
  ,("repetition", Choice [Atom "?", Atom "+", Atom "*",
                          Sequence [Atom "{", optional (RuleX "number"),
                                    Atom ",", optional (RuleX "number"),
                                    Atom "}"]])
  ,("choice", Sequence [RuleX "spaces", Atom "|",
                        RuleX "spaces", Rule "expression"])
  ,("string", Sequence [Atom "\"", anyTimes (RuleX "char"), Atom "\""])
  ,("pos_lookahead", Sequence [Atom "=", Rule "expression"])
  ,("pos_lookahead", Sequence [Atom "!", Rule "expression"])
  ]
