
module GxiMemo.SyntaxNode where

import GxiMemo.MatchData


-- Atom, 'a', "abc"
data Char = Char Prelude.Char

-- Repetition, .+, .*, .?, .{m,n}
data RepetitionTime = Int | Infinity
data Repetition = Repetition RepetitionTime RepetitionTime

-- Alternation, (a|b|c)
data (Expression a) => Alternation a = Alternation [a]

-- Character class, [0-9A-Za-z_]
data CharacterClassMember =
    CharacterRange Prelude.Char Prelude.Char
  | Character Prelude.Char
data CharacterClass = CharacterClass [CharacterClassMember]
data InverseCharacterClass = InverseCharacterClass [CharacterClassMember]

-- Sequence, abc :: [Char, Char, Char]
data (Expression a) => Sequence a = Sequence [a]



class Expression a where
  match :: a -> String -> MatchData
