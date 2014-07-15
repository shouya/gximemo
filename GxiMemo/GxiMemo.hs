-- -*- coding: utf-8 -*-
module GxiMemo where

import Parser

import Control.Monad.State
import Control.Applicative hiding (optional)
import Data.List
import Data.Map as M hiding (map, filter)


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
  ,("rep_mintime", RuleX "number")
  ,("rep_maxtime", RuleX "number")
  ,("rep_mark", Choice [Atom "?", Atom "+", Atom "*",
                        Sequence [Atom "{",
                                  optional (Rule "rep_mintime"),
                                  Atom ",",
                                  optional (Rule "rep_maxtime"),
                                  Atom "}"]])

  ,("repetition", Sequence [Rule "expression",
                            optional $ Rule "rep_mark"])
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


simplify :: MatchData -> MatchData

simplify (MList (xs)) = xs'''
  where notnul x = mToString x /= ""
        xs' = map simplify xs
        xs'' = filter notnul xs'
        xs''' = case length xs'' of
          0 -> MAtom ""
          1 -> head xs''
          _ -> MList xs''

simplify (MPair (n,m)) = MPair (n,simplify m)
simplify x = x


penetrateSubstitute :: String -> MatchData -> MatchData
penetrateSubstitute name xs = case xs of
  MList xs' -> if length xs' == 1 then simplifyGM $ head xs'
               else MPair (name, simplifyGM $ MList xs')
  _         -> simplifyGM xs

{-
Sequence is defined as below:
  sequence = repetition (@spaces_nonl repetition)*
So the resulting match data would be in form of:
  [ele1, [[space?, ele2], [space?, ele3], ...]]
This function transform it into:
  [ele1, ele2, ele3, ...]

* One element sequence will be simplified.
-}

expandSequence :: MatchData -> MatchData
expandSequence (MList xs) = MList ([head xs] ++ map (last . getMList) (tail xs))
expandSequence _ = error "not a sequence"

{-
Repetition:
  expression ("?" | "+" | "*" | "{" @number? "," @number? "}")

Match Data format:
  [expression => expr, rep_mark => "*"]
Or
  [expression => expr, rep_mark => [rep_min => 1, rep_max => 3]]


* A repetition without repetition marks will be simplified.
-}


simplifyGM :: MatchData -> MatchData
--simplifyGM (MList (_:(MPair ("rule", x)):xs)) =
--               simplifyGM (MList ((MPair ("rule", x)):xs))

simplifyGM (MList xs) = MList $ map simplifyGM $ filter (not . isMAtom) xs

-- simplification for 'token' is only for debugging use,
-- and should be removed in production
simplifyGM (MPair ("token",xs)) = MPair ("token", MAtom $ mToString xs)
simplifyGM (MPair ("token_omitted",xs)) =
  MPair ("token_omitted", MAtom $ mToString xs)
simplifyGM (MPair ("string",xs)) = MPair ("string",
                                          MAtom $ tail $ init $ mToString xs)
simplifyGM (MPair ("rule", MList xs)) =
  MPair ("rule", MList ([simplifyGM $ head xs] ++ drop 4 xs'))
  where xs' = map simplifyGM xs

simplifyGM (MPair ("choice",xs)) = penetrateSubstitute "choice" xs
simplifyGM (MPair ("sequence",xs)) = penetrateSubstitute "sequence" xs
simplifyGM (MPair ("repetition",xs)) = penetrateSubstitute "repetition" xs
simplifyGM (MPair ("expression",xs)) = penetrateSubstitute "expression" xs
simplifyGM (MPair ("rep_mark",xs)) = penetrateSubstitute "rep_mark" xs

simplifyGM (MPair (n,x)) = MPair (n,simplifyGM x)
simplifyGM x = x

-- debugParse :: String -> IO (Maybe MatchData, ParsingState)
debugParse start = readFile "Parser.memo" >>= \str ->
  return $ ((evalState (parseI $ main) $ ParsingState str rm) >>=
                 (return . simplifyGM . simplify))
  where rm = fromList rules
        main = rm ! start

debugConvert :: IO ()
debugConvert = readFile "Parser.memo~" >>= \str ->
  case parseToRuleList str of
    Just m  -> putStrLn $ intercalate "\n\n" $ map show m
    Nothing -> putStrLn "Fork! 怎麼又解析出錯了摔!"


extract  :: RuleName -> MatchData -> Maybe MatchData
extract name (MList xs) = find foo xs
  where foo (MPair (name',_)) = if name == name'
                                then True
                                else False
        foo _ = False
extract _ _ = error "invalid"

extractString :: RuleName -> MatchData -> Maybe String
extractString name m = extract name m >>= return . mToString


toPatternPair :: MatchData -> Maybe RulePair
toPatternPair (MPair ("rule",m)) = do
  rulename <- return $ mToString $ snd $ getMPair $ head $ getMList m
  pattern  <- toPattern . last . getMList $ m
  return (rulename, pattern)


toPatternPair x = toPattern x >>= \_ -> Just $ ("error", Atom $ mInspect x)
-- impossible: should be Nothing

toPattern :: MatchData -> Maybe Pattern
toPattern (MPair ("string", str)) = return . Atom . tail . init . mToString $ str
toPattern (MPair ("token", str)) = return . Rule . mToString $ str
toPattern (MPair ("token_omitted", str)) =
  return . RuleX . tail . mToString $ str                    -- skip the '@'
toPattern (MPair ("choice", xs)) = error $ mInspect xs
toPattern (MPair ("sequence", xs)) =
  liftM Sequence (mapM toPattern $ getMList $ expandSequence xs)           -- worked
toPattern (MPair ("expression", xs)) = toPattern xs
toPattern (MPair ("repetition", xs)) = do
  pattern <- expr
  return $ Repetition pattern rep_min rep_max
  where args = getMList xs
        expr = toPattern $ head args
        rep_mark = last args
        (rep_min, rep_max) = case rep_mark of
          MAtom "+" -> (RTInt 1, RTInf)
          MAtom "*" -> (RTInt 0, RTInf)
          MAtom "?" -> (RTInt 0, RTInt 1)
          MList _ -> undefined             -- TODO: -- (RTInt 0, RTInt 1)
          _       -> error "invalid"

toPattern (MList xs) = -- error $ mInspect (MList xs)
  case length xs of
    1 -> toPattern $ head xs
    _ -> Sequence <$> (mapM toPattern xs) -- should never be this case


{-

[token_omitted=>[@/spaces]/[[ /token=>rule]/[ /repetition=>[expression=>[(/sequence=>[repetition=>[token_omitted=>[@/newline]/rep_manytimes=>+]/[ /token=>rule]]/)]/rep_anytimes=>*]]]]

[token_omitted=>[@/spaces]/[ /repetition=>[expression=>[(/sequence=>[repetition=>[token_omitted=>[@/newline]/rep_manytimes=>+]/[ /token=>rule]]/)]/rep_anytimes=>*]

  [token_omitted=>[@/spaces]
   /
   [[ /token=>rule]
    /
    [ /repetition=>
      [expression=>
       [(/
        sequence=>[
          repetition=>[token_omitted=>[@/newline]/rep_manytimes=>+]/[ /token=>rule]]/)]/rep_anytimes=>*]]]

-}

{-
[sequence=>
 [token_omitted=>[@/spaces]
  /
  [[ /token=>rule]
   /
   [ /repetition=>
     [expression=>
      [(/sequence=>
        [repetition=>
         [token_omitted=>
          [@/newline]
          /
          rep_manytimes=>+
         ]
          /
          [ /token=>rule]
         ]/)
       ]
       /
       rep_anytimes=>*
      ]
    ]
  ]
 ]
  ]
-}

-- toPattern (MList xs) = mapM toPattern xs >>= return . Sequence
toPattern x = Just $ Atom $ mInspect x             -- unrecognized
-- toPattern (MPair ("repetition", xs)) =
--      Sequence $ map toPattern $ filter isMPair xs



parseToRuleList :: String -> Maybe [RulePair]
parseToRuleList str = do
  rawmain  <- parse str (fromList rules) "main"
  rawrules <- (return . expandSequence . simplifyGM . simplify) rawmain
--  error (mInspect rawrules)
  case rawrules of
    MList xs -> mapM toPatternPair xs
    _        -> Nothing
