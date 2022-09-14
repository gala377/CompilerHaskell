{
module Syntax.Lexer
    ( toTokens
    , Token ( .. ) 
    ) where

import qualified Data.Text as Text
}

%wrapper "basic"

$digit = 0-9 --digits
$alpha = [a-zA-Z]

tokens :-
    $white+ ;
    "#".*   ;
    $digit+ { \s -> IntLit (read s) }
    $digit+\.$digit* { \s -> FloatLit (read s) }
    "true"  { \_ -> BoolLit True }
    "false" { \_ -> BoolLit False }
    "end"   { \_ -> End }
    "let"   { \_ -> Let }
    "if"    { \_ -> If }
    "else"  { \_ -> Else }
    "while" { \_ -> While }
    "for"   { \_ -> For }
    "function"   { \_ -> Fun }
    "var"   { \_ -> Var }
    "type"  { \_ -> Type }
    "nil"   { \_ -> Nil }
    "array" { \_ -> Array }
    "break" { \_ -> Break }
    "do"    { \_ -> Do }
    "in"    { \_ -> In }
    "of"    { \_ -> Of }
    "then"  { \_ -> Then }
    "to"    { \_ -> To }
    "continue" {\_ -> Continue }
    "="     { \_ -> Assignment }
    "/"     { \_ -> Divide }
    "("     { \_ -> LParen }
    ")"     { \_ -> RParen }
    "{"     { \_ -> LBracket }
    "}"     { \_ -> RBracket }
    "["     { \_ -> LSquareBracket }
    "]"     { \_ -> RSquareBracket }
    "+"     { \_ -> Plus }
    "-"     { \_ -> Minus }
    "*"     { \_ -> Multiplication }
    "!"     { \_ -> Negation }
    "&&"    { \_ -> And }
    "||"    { \_ -> Or }
    "<"     { \_ -> LessThan }
    "<="    { \_ -> LessOrEq }
    ">"     { \_ -> GreaterThan }
    ">="    { \_ -> GreaterOrEq }
    "=="    { \_ -> Equal }
    "!="    { \_ -> NotEqual }
    ","     { \_ -> Comma }
    "."     { \_ -> Dot }
    ";"     { \_ -> Semicolon }
    ":"     { \_ -> Colon }
    _+|_*[$alpha][_$alpha$digit]*    { \s -> Identifier $ Text.pack s }
    \"(\\.|[^\"\\])*\" { \s -> StringLit $ Text.pack $ tail $ init s  }

{
data Token
    {- literals -}
    = IntLit Int
    | FloatLit Double
    | BoolLit Bool
    | StringLit Text.Text
    | Identifier Text.Text
    | Nil
    {- keywords -}
    | If
    | Let
    | Else
    | While
    | For
    | Fun
    | Var
    | Type
    | Array
    | Break
    | Do
    | End
    | In
    | Of
    | Then
    | To
    | Continue
    {- other operators -}
    | Assignment
    | Comma
    | Semicolon
    | Colon
    | Dot
    {- bin operators -}
    | Negation
    | And
    | Or
    {- comp operators -}
    | LessThan
    | GreaterThan
    | LessOrEq
    | GreaterOrEq
    | Equal
    | NotEqual
    {- arithmetic operators -}
    | Plus
    | Minus
    | Divide
    | Multiplication
    {- grouping -}
    | LParen
    | RParen
    | LBracket
    | RBracket
    | LSquareBracket
    | RSquareBracket
    {- special -}
    | EOF
    deriving (Eq, Show)

toTokens :: String -> [Token]
toTokens = alexScanTokens
}