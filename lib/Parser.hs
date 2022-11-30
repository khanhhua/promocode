module Parser where

newtype Parser a = Parser
  { run :: String -> (a, String)
  }