
import GxiMemo.SyntaxNode
import GxiMemo.RuleTable

rules =
  [("main", Sequence [Rule "spaces",
                      anyTimes (Sequence [Rule "rule",
                                          manyTimes $ Rule "newline"]),
                      Rule "rule",
                      Rule "spaces"])
  ,("space", Choice (map Atom [" ", "\t", "\r", "\n"]))
  ,("spaces", anyTimes (Rule "space"))
  ,("alpha", Choice (map (Atom . show) (['a'..'z'] ++ ['A'..'Z'])))
  ,("alpha_underscore", Choice [Rule "alpha", Atom "_"])
  ,("digit", Choice (map (Atom . show) ['0'..'9']))
  ,("newline", Choice (map Atom ["\r\n", "\r", "\n"]))
  ,("token", Sequence [Rule "alpha",
                       anyTimes (Choice [Rule "alpha", Rule "digit"])])
  ,("rule", Sequence [Rule "token", Rule "spaces", Atom "="])
   --
  ]
