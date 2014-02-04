
import GxiMemo.SyntaxNode

rules =
  [("main", Sequence [Rule "spaces",
                      anyTimes (Sequence [Rule "rule",
                                          manyTimes $ Rule "newline"]),
                      Rule "rule",
                      Rule "spaces"])
  ,("space", Choice (map Atom [" ", "\t", "\r", "\n"]))
  ,("spaces", anyTimes (Rule "space"))
  ,("newline", Choice (map Atom ["\r\n", "\r", "\n"]))
  ,("alpha", Choice (map (Atom . (:[])) (['a'..'z'] ++ ['A'..'Z'])))
  ,("alpha_underscore", Choice [Rule "alpha", Atom "_"])
  ,("digit", Choice (map (Atom . (:[])) ['0'..'9']))
  ,("integer", manyTimes (Rule "digit"))
  ,("token", Sequence [Rule "alpha",
                       anyTimes (Choice [Rule "alpha", Rule "digit"])])
  ,("rule", Sequence [Rule "spaces", Rule "token",
                      Rule "spaces", Atom "=",
                      Rule "spaces", Rule "sequence", Rule "spaces"])
  ,("seequence", Sequence [anyTimes (Sequence [Rule "expression", Rule "spaces"]),
                           Rule "expression"])
  ,("expression", Choice [Sequence [Atom "(", Rule "spaces", Rule "sequence",
                                    Rule "spaces", Atom ")"],
                          Rule "repetition", -- left-recursion
                          Rule "choice",
                          Rule "pos_lookahead",
                          Rule "neg_lookahead",
                          Rule "string",
                          Rule "rule"])
  ,("string", Sequence [Atom "\"", anyTimes (Rule "char"), Atom "\""])
  ,("char", Choice ([Rule "alpha", Rule "digit"] ++
                    (map (Atom . (:[])) "~`!@#$%^&*()-_=+[]{}:\";'<>?,./") ++
                    [Atom "\\\\", Atom "\\\"",
                     Sequence [Atom "\\", Rule "alpha"]]))
  ,("repetition", Choice [Sequence [Rule "expression", Atom "?"],
                          Sequence [Rule "expression", Atom "+"],
                          Sequence [Rule "expression", Atom "*"],
                          Sequence [Rule "expression", Atom "{",
                                    optional (Rule "number"), Atom ",",
                                    optional (Rule "number"), Atom "}"]])
  ,("choice", Sequence [Rule "expression", Rule "spaces", Atom "|",
                        Rule "spaces", Rule "expression"])
  ,("pos_lookahead", Sequence [Atom "@", Rule "expression"])
  ,("pos_lookahead", Sequence [Atom "!", Rule "expression"])
  ]
