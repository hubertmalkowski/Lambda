module Main where

import Data.Bifunctor (first)
import Data.Map
import Eval
import Parser
import System.Console.Haskeline
import Text.Megaparsec

parseAndRun :: String -> Env -> Either String (String, Env)
parseAndRun input env = do
  ast <- first errorBundlePretty $ parse pl "" input
  (val, newenv) <- evalProgram env ast
  case val of
    Just x -> Right (show x, newenv)
    Nothing -> Right ("", newenv)

main :: IO ()
main = runInputT defaultSettings (loop Data.Map.empty)
  where
    loop :: Env -> InputT IO ()
    loop env = do
      minput <- getInputLine "λ> "
      case minput of
        Nothing -> return ()
        Just "quit" -> outputStrLn "Goodbye!"
        Just input -> case parseAndRun input env of
          Left e -> do
            outputStrLn e
            loop env
          Right (val, newnev) -> do
            outputStrLn val
            loop newnev
