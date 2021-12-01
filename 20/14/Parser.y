{
module Parser (Exp(..), parse, parseLex) where

import Lexer (Token(..), runLex)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
        mem  { TokenMem }
        '['  { TokenLeftBrace }
        ']'  { TokenRightBrace }
        '='  { TokenEq }
        int  { TokenNum $$ }
        mask { TokenMask }

%%

E : Exps { reverse $1 }

Exps : Exp      { [$1] }
     | Exps Exp { $2 : $1 }

Exp : mask '=' int            { Mask $3 }
    | mem '[' int ']' '=' int { Mem (read $3) (read $6) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Mask String |
           Mem Int Int
        deriving (Show)

parseLex = parse. runLex

main = getContents >>= print . parse . runLex
}
