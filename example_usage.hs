
import GxiMemo.Main

syntax :: Syntax
syntax = Syntax Seqence [
          ZeroOrOne  (Alternation [Char '+', Char '-']),
          ZeroOrMany (CharacterClass [CharacterRange '0' '9']),
          ZeroOrOne  (Sequence [Char '.',
                                CharacterClass [CharacterRange '0' '9']])
