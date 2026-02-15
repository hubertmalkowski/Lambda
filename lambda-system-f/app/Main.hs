module Main where

import Check (Env, Typ, checkProgram, initEnv)
import Data.Bifunctor (first)
import Parser
import System.Console.Haskeline
import Text.Megaparsec

printError :: Either (SourcePos, String) (Typ, Env) -> Either [Char] (Typ, Env)
printError (Left (pos, err)) = Left (sourcePosPretty pos ++ "\n" ++ err)
printError (Right other) = Right other

parseCheckRun :: String -> Env -> Either [Char] (String, Env)
parseCheckRun input typeEnv = do
  ast <- first errorBundlePretty $ parse pl "" input
  (typ, newtypenv) <- printError $ checkProgram typeEnv ast
  Right (show typ, newtypenv)

main :: IO ()
main = runInputT defaultSettings (loop initEnv)
  where
    loop :: Env -> InputT IO ()
    loop env = do
      minput <- getInputLine "λ2> "
      case minput of
        Nothing -> return ()
        Just "quit" -> outputStrLn "Goodbye!"
        Just input -> case parseCheckRun input env of
          Left e -> do
            outputStrLn e
            loop env
          Right (val, newnev) -> do
            outputStrLn val
            loop newnev
