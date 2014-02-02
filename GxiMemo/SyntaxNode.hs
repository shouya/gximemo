
module GxiMemo.SyntaxNode where

import GxiMemo.MatchData


-- Atom, 'a', "abc"
data Char = Char Prelude.Char
          deriving (Show)
data CharMatchData =


-- Repetition, .+, .*, .?, .{m,n}
data RepetitionTime = Integer Integer | Infinity
                    deriving (Show)
type MinTime = RepetitionTime
type MaxTime = RepetitionTime
data Repetition a = Repetition MinTime MaxTime a
                  deriving (Show)

data RepetitionMatchData a = RepetitionMatchData
                             { klass         :: Repetition a
                             , matchedString :: String
                             , matchedTime   :: RepeititonTime
                             }

-- Alternation, (a|b|c)
data Alternation a = Alternation [a]
                   deriving (Show)

-- Character class, [0-9A-Za-z_]
data CharacterClassMember =
    CharacterRange Prelude.Char Prelude.Char
  | Character Prelude.Char
  deriving (Show)

data CharacterClass = CharacterClass [CharacterClassMember]
                    deriving (Show)
data InverseCharacterClass = InverseCharacterClass [CharacterClassMember]
                           deriving (Show)

-- Sequence, abc :: [Char, Char, Char]
data Sequence a = Sequence [a]
                deriving (Show)



-----------------------
-- Class: Expression --
-----------------------
class Matchable a where
  match :: a -> MatchData -> MatchResult



-----------------------------
-- Instance of SyntaxNodes --
-----------------------------
instance Matchable GxiMemo.SyntaxNode.Char where
  match (Char c) m = if c == (head $ residual m)
                     then return $ matchLen m 1
                     else Nothing


instance Matchable CharacterClass where
  match (CharacterClass (x:xs)) m =
    case match x m of
      Just m  -> return $ matchLen m 1
      Nothing -> match (CharacterClass xs) m

  match (CharacterClass []) _ = Nothing


instance Matchable CharacterClassMember where
  match (Character c) m        = if c == (head $ residual m)
                                 then return $ matchLen m 1
                                 else Nothing
  match (CharacterRange a b) m = if a <= x && x <= b
                                 then return $ matchLen m 1
                                 else Nothing
    where x = head $ residual m

instance (Matchable a) => Matchable (Sequence a) where
  match (Sequence []) m = return $ matchLen m 0
  match (Sequence seq@(x:xs)) m =
    match x m >>= \m ->
    match (Sequence xs) (matchLen m (matchLength m)) >>= \m' ->
    return $ MatchData (residual m') (matchLength m' + matchLength m) (offset m')


--instance Expression Repetition where
--   match (Repetition min max a) s
--     | min == max = matchtime min a s
--     | otherwise  = matchonce >> matchtime
--   where matchtime = Nothing
