import IO
import Language.Core.Parser
import Language.Core.ParseGlue

main = do
  file <- openFile "B.hcr" ReadMode
  contents <- hGetContents file
  case parse contents 0 of
    (FailP e) -> do putStrLn "HORRIBLY WRONG"
                    putStrLn e
    (OkP e) -> putStrLn $ show e
  return ()
