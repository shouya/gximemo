# Ĝi Memo (GxiMemo)
Ĝi kredas ĝi memo: yet another compiler compiler. (PEGs)

## Description
### What is this project
GxiMemo is a project that aims to construct a PEG compiler that
is capable to compile its own language specification.

This project is mostly a toy, because although it works, it is not as
practically usable like other excellent compilers in Haskell such as
Parsec.

### Why is this project
Abstraction is attractive to me. Yet I finished my previous project,
"revo", a functionally complete scheme interpreter, I started to think
beyond what I can do next. The idea of building a compiler compiler rose
at that time, since it seems a compiler compiler is a kind of, at
least a part of, meta-compiler.

### Ĝi kredas ĝi memo
In Esperanto, "Ĝi kredas ĝi memo" means "It creates itself".
The aphorism describes such a self-compiling process, and that
explains how GxiMemo works.

Replacing the subject of "Ĝi kredas ĝi memo" with a hole, we can
construct an interesting quinification of the sentence.

> "X kredas ĝi memo" kredas ĝi memo.

## Details
### Components of GxiMemo
GxiMemo mainly separates into three parts, the parser, the
language, and the specification. The different components are
related to the other.

![gximemo_components](https://raw.githubusercontent.com/shouya/gximemo/assets/components.svg)

The parser part (`Parser.hs`) provides a complete PEG parser. It can be fed with a
group of parsing rules and a bunch of input text, then produce a
matched result.

The GxiMemo language part (`GxiMemo.hs`) contains a full class of
specification rules for its own language, and the conversion functions
that converts the matched result into the specification rules. The
process of translating the matched data into specification is the key
of this project. With this part it would be able to define a BNF
specification in a succinct and convenient syntax.

Besides, there is a language specification file (`Gxi.memo`). The file
contains a group of language specification describing itself. Parsing
which with the previous two parts, it will get a completely same rule
set as that used to parse itself specified in `GxiMemo.hs`.


### Interfaces

Below are the main interfaces exported and are available for advanced
usages.

#### `Parser.hs`
```haskell
type RuleName = String
type RulePair = (RuleName, Pattern)
type RuleMap = M.Map RuleName Pattern

data RepTime = RTInt Integer -- n-times
             | RTInf         -- infinity
             deriving (Show,Eq)

manyTimes :: Pattern -> Pattern
anyTimes :: Pattern -> Pattern
optional :: Pattern -> Pattern

rtZerop :: RepTime -> Bool
rtPred :: RepTime -> RepTime

data Pattern = Atom String
             | Rule String
             | RuleX String  -- rule that only do matching but ignoring the result
             | Repetition Pattern RepTime RepTime -- pat, minTime, maxTime
             | Choice [Pattern]
             | Sequence [Pattern]
             | NegativeLookahead Pattern
             | PositiveLookahead Pattern
             deriving (Eq)

instance Show Pattern

data MatchData = MAtom String
               | MList [MatchData]
               | MPair (RuleName, MatchData)
               deriving (Eq)

instance Show MatchData

isMPair :: MatchData -> Bool
getMPair :: MatchData -> (RuleName, MatchData)
isMList :: MatchData -> Bool
getMList :: MatchData -> [MatchData]
isMAtom :: MatchData -> Bool
getMAtom :: MatchData -> String

mInspect :: MatchData -> String
mToString :: MatchData -> String

parseI :: Pattern -> ParseResult
```

### `GxiMemo.hs`
```haskell
rules :: [RulePair]
parse :: String -> RuleMap -> String -> Maybe MatchData
simplify :: MatchData -> MatchData
simplifyGM :: MatchData -> MatchData
toPatternPair :: MatchData -> Maybe RulePair
toPattern :: MatchData -> Maybe Pattern
parseToRuleList :: String -> Maybe [RulePair]
```

Use `parseToRuleList` to convert a specification string into rule
pairs. And then use `parse`, fed with the text to be parsed, rule map
build from rule pairs, and the starting rule name, to generate a match result.

### Demonstration
A demonstration of the GxiMemo language spec conversion has been shown
in `Main.hs`. Enter the following command to run it.

```bash
$ cd GxiMemo
$ runghc Main.hs
```

## License

The MIT License (MIT)

Copyright (c) 2013-2014 Shou Ya

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
