{
module Main (main) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                          ; -- white spaces
  "#".*                            ; -- single-line comment
  "#"!   [.\n]*  !"#"              ; -- multi-line comment

  -- blocks
  \{                               { \s -> LBrace }
  \}                               { \s -> RBrace }
  \(                               { \s -> LParen }
  \)                               { \s -> RParen }
  \[                               { \s -> LBracket }
  \]                               { \s -> RBracket }
 
  -- sections
  \@imports                        { \s -> Imports } 
  \@globals                        { \s -> Globals }
  \@subprograms                    { \s -> Subprograms }
  \@main                           { \s -> Main }
  
  -- filename
  [$alpha\_$digit]+   \.  [a-z]+   { \s -> Filename s }
  
  -- control
  ";"                              { \s -> SemiColon }
  $digit+                          { \s -> Int (read s) }
 
  -- operatos
  =                                { \s -> Assign }
  \+                               { \s -> Plus }
  \-                               { \s -> Minus }
  \*                               { \s -> Asterisk }
  \/                               { \s -> Bar}
  "+="                             { \s -> PlusEquals }
  "-="                             { \s -> MinusEqual }
  "*="                             { \s -> AsteriskEquals }
  "/="                             { \s -> BarEquals }
  "++"                             { \s -> PlusPlus }
  "--"                             { \s -> MinusMinus }
  "<"                              { \s -> LessThan }
  ">"                              { \s -> GreaterThan }
  "<="                             { \s -> LessEquals }
  ">="                             { \s -> GreaterEquals }
  "!"                              { \s -> Negation }
  "&&"                             { \s -> AndShortCircuit }
  "and"                            { \s -> And }
  "||"                             { \s -> OrShortCircuit }
  "or"                             { \s -> Or }

  -- conditionals
  if                               { \s -> If }
  else                             { \s -> Else }
  "else if"                        { \s -> ElseIf } 

  -- types
  int                              { \s -> Type s }
  float                            { \s -> Type s }
  set                              { \s -> Type s }
  double                           { \s -> Type s } 
  bool                             { \s -> Type s }
  tuple                            { \s -> Type s }
  byte                             { \s -> Type s }

  -- loops
  while                            { \s -> Loop s }
  for                              { \s -> Loop s }

  -- names 
  $alpha [$alpha $digit \_ \']*    { \s -> Id s }

{
-- The token type:
data Token =

  -- blocks
  LBrace           |
  RBrace           |
  LParen           |
  RParen           |
  LBracket         |
  RBracket         |
  
  -- sections
  Imports          |
  Globals          |
  Subprograms      |
  
  Filename String  |
  
  SemiColon        |
  
  -- operators
  Assign           |
  Plus             |
  Minus            |
  Asterisk         |
  Bar              |
  PlusEquals       |
  MinusEqual       |
  AsteriskEquals   |
  BarEquals        |
  PlusPlus         |
  MinusMinus       |
  LessThan         |
  GreaterThan      |
  LessEquals       |
  GreaterEquals    |
  Negation         |
  AndShortCircuit  |
  And              |
  OrShortCircuit   |
  Or               |
  
  -- consitionals
  If               |
  Else             |
  ElseIf           |

  -- types
  Type String      |
  
  -- loops
  Loop String      |
  
  -- identifier
  Id String        |
  Int Int          |
  Main
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}