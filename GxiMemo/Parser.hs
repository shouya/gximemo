
import GxiMemo.SyntaxNode

rules =
  [("main", Sequence [Rule "spaces", Rule "rule",
                      anyTimes (Sequence [manyTimes (Rule "newline"),
                                          Rule "rule"])])
  ,("space", Choice (map Atom [" ", "\t", "\r\n", "\r", "\n"]))
  ,("space_nonl", Choice (map Atom [" ", "\t"]))
  ,("spaces", anyTimes (Rule "space"))
  ,("spaces_nonl", anyTimes (Rule "space_nonl"))
  ,("newline", Choice (map Atom ["\r\n", "\r", "\n"]))
  ,("alpha", Choice (map (Atom . (:[])) (['a'..'z'] ++ ['A'..'Z'])))
  ,("alpha_underscore", Choice [Rule "alpha", Atom "_"])
  ,("digit", Choice (map (Atom . (:[])) ['0'..'9']))
  ,("integer", manyTimes (Rule "digit"))
  ,("token", Sequence [Rule "alpha_underscore",
                       anyTimes (Choice [Rule "alpha_underscore", Rule "digit"])])
  ,("char", Choice ([Rule "alpha", Rule "digit"] ++
                    (map (Atom . (:[])) " ~`!@#$%^&*()-_=+[]{}:;'<>?,./|") ++
                    [Atom "\\\\", Atom "\\\"",
                     Sequence [Atom "\\", Rule "alpha"]]))


  ,("rule", Sequence [Rule "spaces", Rule "token",
                      Rule "spaces", Atom "=",
                      Rule "spaces", Rule "sequence", Rule "spaces_nonl"])
  ,("sequence", Sequence [Rule "expression",
                          anyTimes (Sequence [Rule "spaces_nonl",
                                              Rule "expression"])])
  ,("expression", Sequence [Rule "expression1", anyTimes (Rule "choice_mark")])
  ,("expression1", Sequence [Rule "expression2",
                             optional (Rule "repetition_mark")])
  ,("expression2", Choice [Sequence [Atom "(", Rule "spaces", Rule "sequence",
                                     Rule "spaces", Atom ")"],
                           Rule "string",
                           Rule "token",
                           Rule "pos_lookahead",
                           Rule "neg_lookahead"])
  ,("repetition_mark", Choice [Atom "?", Atom "+", Atom "*",
                               Sequence [Atom "{", optional (Rule "number"),
                                         Atom ",", optional (Rule "number"),
                                         Atom "}"]])
  ,("choice_mark", Sequence [Rule "spaces", Atom "|",
                             Rule "spaces", Rule "expression"])
  ,("string", Sequence [Atom "\"", anyTimes (Rule "char"), Atom "\""])
  ,("pos_lookahead", Sequence [Atom "@", Rule "expression"])
  ,("pos_lookahead", Sequence [Atom "!", Rule "expression"])
  ]
