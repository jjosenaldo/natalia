{
module Main (main) where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                         ; -- white spaces
  "#".*                           ; -- single-line comment
  "#"!   [.\n]*  !"#"             ; -- multi-line comment
  \{                              {\s -> LBrace}
  \}                              {\s -> RBrace}
  imports                         {\s -> Imports} 
  globals                         {\s -> Globals}
  subprograms                     {\s -> Subprograms}
  main                            {\s -> Main}
  [$alpha\_$digit]+   \.  [a-z]+  {\s -> Filename s}
  ";"                             {\s -> SemiColon}


{
-- The token type:
data Token =
  LBrace |
  RBrace     |
  Imports   |
  Globals     |
  Subprograms   |
  Filename String |
  SemiColon |
  Main
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}