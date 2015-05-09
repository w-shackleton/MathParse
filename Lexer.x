{
module Lexer where
}

%wrapper "basic"

$digit = 0-9

tokens :- 
  $white+ { \s -> White }
  "*"     { \_ -> Mul }
  "/"     { \_ -> Div }
  "+"     { \_ -> Plus }
  "-"     { \_ -> Minus }
  "("     { \_ -> LParen }
  ")"     { \_ -> RParen }
  $digit+ { \s -> Num (read s) }
  

{
data Token = Num Int | Plus | Minus | Mul | Div | LParen | RParen | White | EOF
    deriving (Eq, Show)

mathLex = (++ [EOF]) . filter (/= White) . alexScanTokens
}
