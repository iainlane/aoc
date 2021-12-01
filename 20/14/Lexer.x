{
module Lexer (Token(..), runLex) where
}

%wrapper "basic"

$digit = [X0-9]

tokens :-

  $white+;
  \[ { \s -> TokenLeftBrace }
  \] { \s -> TokenRightBrace }
  = { \s -> TokenEq }
  mem { \s -> TokenMem }
  $digit+ {\s -> TokenNum s }
  mask {\s -> TokenMask}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        TokenLeftBrace |
        TokenRightBrace |
        TokenEq |
        TokenMem |
        TokenNum String |
        TokenMask
        deriving (Eq,Show)

runLex = alexScanTokens

main = do
  s <- getContents
  print (alexScanTokens s)
}
