module Main where

import Parser
import GxiMemo


printRulePairs :: [RulePair] -> IO ()
printRulePairs = mapM_ (\(n,p) -> putStrLn $ (n ++ " = " ++ show p))

main :: IO ()
main = do
  putStrLn "=============== RULES PREDEFINED ======================"
  printRulePairs rules
  putStrLn ""
  putStrLn "=============== RULES PARSED & CONVERTED =============="
  readFile "Gxi.memo" >>= \str ->
    case parseToRuleList str of
      Just m  -> printRulePairs (reverse m)
      Nothing -> putStrLn ("非常抱歉，本串因為如下原因導致無法正常解析: \n\n" ++
                           "字串內容不和諧，\n" ++
                           "侵犯原作者著作權，\n\n" ++
                           "        -- GxiMemo GxiMemo Parser")
